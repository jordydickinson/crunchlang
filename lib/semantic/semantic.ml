exception Unbound_identifier of {
    loc: Srcloc.t;
    ident: string;
  }
[@@deriving sexp]

exception Unbound_type of {
    loc: Srcloc.t;
    ident: string;
  }
[@@deriving sexp]

exception Type_error of {
    loc: Srcloc.t;
    expected: Type.t list;
    got: Type.t;
  }
[@@deriving sexp]

exception Arity_mismatch of {
    loc: Srcloc.t;
    expected: int;
    got: int;
  }
[@@deriving sexp]

exception Purity_error of {
    loc: Srcloc.t
  }
[@@deriving sexp]

exception Not_assignable of {
    loc: Srcloc.t;
  }
[@@deriving sexp]

module Env : sig
  type binding = {
    typ: Type.t;
    pure: bool;
  }

  type type_binding = {
    n_params: int;
    constructor: Type.t array -> Type.t
  }

  type t

  val empty : t

  val prelude : t

  val bind : t -> ident:string -> typ:Type.t -> pure:bool -> t
  val lookup : t -> string -> binding option
  val typeof : t -> string -> Type.t option

  val bind_type : t -> ident:string -> typ:Type.t -> t
  val lookup_type : t -> string -> type_binding option
end = struct
  type binding = {
    typ: Type.t;
    pure: bool;
  }

  type type_binding = {
    n_params: int;
    constructor: Type.t array -> Type.t;
  }

  type t = {
    vars: binding String.Map.t;
    types: type_binding String.Map.t;
  }

  let empty = {
    vars = String.Map.empty;
    types = String.Map.empty;
  }

  let lookup env ident = Map.find env.vars ident

  let typeof env ident =
    Option.map (lookup env ident) ~f:(function { typ; _ } -> typ)

  let lookup_type env ident = Map.find env.types ident

  let bind env ~ident ~typ ~pure =
    { env with vars = Map.set env.vars ~key:ident ~data:{ typ; pure } }

  let bind_type_constructor env ~ident ~n_params ~constructor =
    { env with types = Map.set env.types ~key:ident ~data:{ n_params; constructor } }

  let bind_type env ~ident ~typ =
    bind_type_constructor env ~ident ~n_params:0 ~constructor:(fun _ -> typ)

  let prelude =
    empty
    |> bind_type ~ident:"void" ~typ:Type.void
    |> bind_type ~ident:"int64" ~typ:Type.int64
    |> bind_type ~ident:"bool" ~typ:Type.bool
    |> bind_type ~ident:"float" ~typ:Type.float
    |> bind_type_constructor ~ident:"array" ~n_params:1
      ~constructor:(fun args -> Type.array args.(0))
end

module Type = struct
  include Type

  module Builder = struct
    type nonrec t = Env.t -> t

    let fun_ ~params ~ret = fun env ->
      let ret = ret env in
      let params = List.map params ~f:(fun param -> param env) in
      fun_ ~params ~ret

    let rec of_ast (type_expr: Ast.Type_expr.t) = fun env ->
      let loc, ident, args =
        match type_expr with
        | Apply { loc; ident; args } -> loc, ident, args
        | Name { loc; ident } -> loc, ident, [||] in
      match Env.lookup_type env ident with
      | None -> raise @@ Unbound_type { loc; ident }
      | Some { n_params; constructor } when n_params = Array.length args ->
        constructor (Array.map args ~f:(fun arg -> of_ast arg env))
      | Some { n_params; _ } ->
        raise @@ Arity_mismatch {
          loc;
          expected = n_params;
          got = Array.length args
        }
  end

  type builder = Builder.t
end

module Expr = struct
  module Bop = Ast.Expr.Bop

  type t =
    | Int of {
        loc: Srcloc.t;
        value: int64;
      }
    | Bool of {
        loc: Srcloc.t;
        value: bool;
      }
    | Float of {
        loc: Srcloc.t;
        value: float;
      }
    | Name of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        pure: bool;
      }
    | Array of {
        loc: Srcloc.t;
        elts: t array;
        elt_type: Type.t;
      }
    | Binop of {
        loc: Srcloc.t;
        op: Bop.t;
        lhs: t;
        rhs: t;
      }
    | Call of {
        loc: Srcloc.t;
        callee: t;
        args: t list;
      }
    | Let_in of {
        loc: Srcloc.t;
        ident: string;
        binding: t;
        body: t;
      }
  [@@deriving sexp_of, variants]

  type builder = Env.t -> t

  let loc = function
    | Int { loc; _ }
    | Bool { loc; _ }
    | Float { loc; _ }
    | Name { loc; _ }
    | Array { loc; _ }
    | Binop { loc; _ }
    | Call { loc; _ }
    | Let_in { loc; _ } -> loc

  let rec typ = function
    | Int _ -> Type.int64
    | Bool _ -> Type.bool
    | Float _ -> Type.float
    | Array { elt_type; _ } -> Type.array elt_type
    | Name { typ; _ } -> typ
    | Binop { lhs; _ } -> typ lhs
    | Call { callee; _ } -> Type.ret_exn @@ typ callee
    | Let_in { body; _ } -> typ body

  let rec impurities = function
    | Int _ | Bool _ | Float _ -> String.Set.empty
    | Name { ident; pure; _ } ->
      if pure then String.Set.empty else String.Set.singleton ident
    | Array { elts; _ } ->
      String.Set.union_list (Array.map elts ~f:impurities |> Array.to_list)
    | Binop { lhs; rhs; _ } -> Set.union (impurities lhs) (impurities rhs)
    | Call { callee; args; _ } ->
      String.Set.union_list (impurities callee :: List.map ~f:impurities args)
    | Let_in { body; _ } -> impurities body

  let is_pure expr = Set.is_empty @@ impurities expr

  let is_lvalue = function
    | Name { pure = false; _} -> true
    | Name _ -> false
    | Int _ | Bool _ | Float _
    | Array _ | Binop _ | Call _
    | Let_in _ -> false

  let check_assignable expr =
    if not @@ is_lvalue expr
    then raise @@ Not_assignable { loc = loc expr }

  let typecheck_or expr ~types =
    if not @@ List.exists types ~f:(fun typ' -> Type.equal typ' @@ typ expr)
    then raise @@ Type_error {
        loc = loc expr;
        expected = types;
        got = typ expr;
      }

  let typecheck expr ~typ:typ' =
    typecheck_or expr ~types:[typ']

  let int ~loc ~value = fun _ -> int ~loc ~value

  let bool ~loc ~value = fun _ -> bool ~loc ~value

  let float ~loc ~value = fun _ -> float ~loc ~value

  let name ~loc ~ident = fun env ->
    match Env.lookup env ident with
    | Some { typ; pure } -> name ~loc ~ident ~typ ~pure
    | None -> raise @@ Unbound_identifier { loc; ident }

  let array ~loc ~elts =
    if Array.is_empty elts then
      array ~loc ~elts ~elt_type:Type.void
    else begin
      let elt_type = typ elts.(0) in
      for i = 1 to Array.length elts - 1 do
        typecheck ~typ:elt_type elts.(i)
      done;
      array ~loc ~elts ~elt_type
    end

  let array ~loc ~elts = fun env ->
    let elts = Array.map elts ~f:(fun elt -> elt env) in
    array ~loc ~elts

  let binop ~loc ~op:Bop.Add ~lhs ~rhs =
    typecheck_or lhs ~types:[Type.int64; Type.float];
    typecheck rhs ~typ:(typ lhs);
    binop ~loc ~op:Bop.Add ~lhs ~rhs

  let binop ~loc ~op ~lhs ~rhs = fun env ->
    binop ~loc ~op ~lhs:(lhs env) ~rhs:(rhs env)

  let call ~loc ~callee ~args =
    begin
      match List.iter2 (Type.params_exn @@ typ callee) args
              ~f:(fun typ arg -> typecheck arg ~typ)
      with
      | Ok _ -> ()
      | Unequal_lengths ->
        raise @@ Arity_mismatch {
          loc;
          expected = List.length @@ Type.params_exn @@ typ callee;
          got = List.length args;
        }
    end;
    call ~loc ~callee ~args

  let call ~loc ~callee ~args = fun env ->
    call ~loc ~callee:(callee env) ~args:(List.map args ~f:(fun arg -> arg env))

  let let_in ?binding_type ~loc:loc' ~ident ~binding ~body =
    Option.iter binding_type ~f:(fun typ -> typecheck ~typ binding);
    if not @@ is_pure binding
    then raise @@ Purity_error { loc = loc binding };
    (let_in) ~loc:loc' ~ident ~binding ~body

  let let_in ?binding_type ~loc ~ident ~binding ~body = fun env ->
    let binding_type = Option.map binding_type ~f:(fun typ -> typ env) in
    let binding = binding env in
    let env = Env.bind env ~ident ~typ:(typ binding) ~pure:true in
    let body = body env in
    (let_in) ?binding_type ~loc ~ident ~binding ~body

  let rec build_ast (expr: Ast.Expr.t) =
    match expr with
    | Int { loc; value } -> int ~loc ~value
    | Bool { loc; value } -> bool ~loc ~value
    | Float { loc; value } -> float ~loc ~value
    | Name { loc; ident } -> name ~loc ~ident
    | Array { loc; elts } -> array ~loc ~elts:(Array.map elts ~f:build_ast)
    | Binop { loc; op; lhs; rhs } ->
      let lhs = build_ast lhs in
      let rhs = build_ast rhs in
      binop ~loc ~op ~lhs ~rhs
    | Call { loc; callee; args } ->
      let callee = build_ast callee in
      let args = List.map args ~f:build_ast in
      call ~loc ~callee ~args
    | Let_in { loc; ident; typ; binding; body } ->
      let binding_type = Option.map ~f:Type.Builder.of_ast typ in
      let binding = build_ast binding in
      let body = build_ast body in
      (let_in) ~loc ~ident ?binding_type ~binding ~body
end

module Stmt = struct
  type t =
    | Expr of Expr.t
    | Block of t list
    | Assign of {
        loc: Srcloc.t;
        dst: Expr.t;
        src: Expr.t;
      }
    | Let of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Var of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | If of {
        loc: Srcloc.t;
        cond: Expr.t;
        iftrue: t;
        iffalse: t option [@sexp.option];
      }
    | Return of {
        loc: Srcloc.t;
        arg: Expr.t option [@sexp.option];
      }
  [@@deriving sexp_of, variants]

  type builder = Env.t -> (t * Env.t)

  let rec impurities stmt =
    let rec block_impurities = function
      | [] -> String.Set.empty
      | Var { ident; _ } :: stmts ->
        Set.remove (block_impurities stmts) ident
      | stmt :: stmts ->
        Set.union (impurities stmt) (block_impurities stmts)
    in
    match stmt with
    | Expr expr -> Expr.impurities expr
    | Block stmts -> block_impurities stmts
    | Assign { dst; src; _ } -> Set.union (Expr.impurities dst) (Expr.impurities src)
    | Let _ -> String.Set.empty
    | Var { binding; _ } -> Expr.impurities binding
    | If { cond; iftrue; iffalse; _ } ->
      String.Set.union_list [
        Expr.impurities cond;
        impurities iftrue;
        Option.value_map iffalse ~f:impurities ~default:String.Set.empty
      ]
    | Return { arg; _ } -> Option.value_map arg ~f:Expr.impurities ~default:String.Set.empty

  let is_pure stmt = Set.is_empty @@ impurities stmt

  let to_block = function
    | Block _ as stmt -> stmt
    | stmt -> Block [stmt]

  let expr expr' = fun env -> expr @@ expr' env, env

  let block stmts = fun env ->
    let rec block' ~accum env = function
      | [] -> List.rev accum
      | stmt :: stmts ->
        let stmt, env = stmt env in
        block' env stmts ~accum:(stmt :: accum)
    in
    block @@ block' env stmts ~accum:[], env

  let assign ~loc ~src ~dst =
    Expr.check_assignable dst;
    Expr.typecheck dst ~typ:(Expr.typ src);
    assign ~loc ~src ~dst

  let assign ~loc ~dst ~src = fun env ->
    assign ~loc ~dst:(dst env) ~src:(src env), env

  let let_ ~loc ~typ ~ident ~binding =
    if Fn.non Expr.is_pure binding
    then raise @@ Purity_error { loc = Expr.loc binding };
    Expr.typecheck ~typ binding;
    (let_) ~loc ~ident ~typ ~binding

  let let_ ~loc ~typ ~ident ~binding = fun env ->
    let typ = Option.map typ ~f:(fun typ -> typ env) in
    let binding = binding env in
    let typ = Option.value typ ~default:(Expr.typ binding) in
    let env = Env.bind env ~ident ~typ:(Expr.typ binding) ~pure:true in
    (let_) ~loc ~typ ~ident ~binding, env

  let var ~loc ~typ ~ident ~binding =
    Expr.typecheck ~typ binding;
    var ~loc ~ident ~typ ~binding

  let var ~loc ~typ ~ident ~binding = fun env ->
    let typ = Option.map typ ~f:(fun typ -> typ env) in
    let binding = binding env in
    let typ = Option.value typ ~default:(Expr.typ binding) in
    let env = Env.bind env ~ident ~typ:(Expr.typ binding) ~pure:false in
    var ~loc ~typ ~ident ~binding, env

  let if_ ~loc ~cond ~iftrue ~iffalse = fun env ->
    let cond = cond env in
    let iftrue, _ = iftrue env in
    let iffalse = match iffalse with
      | Some iffalse -> Some (fst @@ iffalse env)
      | None -> None in
    if_ ~loc ~cond ~iftrue ~iffalse, env

  let return ~loc ~arg = fun env ->
    let arg = Option.map arg ~f:(fun arg -> arg env) in
    let arg_type = Option.value_map arg ~f:Expr.typ ~default:Type.void in
    let ret_type = Env.typeof env "return" |> Option.value_exn in
    if not @@ Type.equal ret_type arg_type
    then raise @@ Type_error {
        loc;
        expected = [ret_type];
        got = arg_type
      };
    return ~loc ~arg, env

  let rec build_ast (stmt: Ast.Stmt.t) =
    match stmt with
    | Expr expr' -> expr @@ Expr.build_ast expr'
    | Block stmts -> block @@ List.map stmts ~f:build_ast
    | Assign { loc; dst; src } ->
      let dst = Expr.build_ast dst in
      let src = Expr.build_ast src in
      assign ~loc ~dst ~src
    | Let { loc; ident; typ; binding } ->
      let typ = Option.map ~f:Type.Builder.of_ast typ in
      let binding = Expr.build_ast binding in
      (let_) ~loc ~ident ~typ ~binding
    | Var { loc; ident; typ; binding } ->
      let typ = Option.map ~f:Type.Builder.of_ast typ in
      let binding = Expr.build_ast binding in
      var ~loc ~ident ~typ ~binding
    | If { loc; cond; iftrue; iffalse } ->
      let cond = Expr.build_ast cond in
      let iftrue = build_ast iftrue in
      let iffalse = Option.map iffalse ~f:build_ast in
      if_ ~loc ~cond ~iftrue ~iffalse
    | Return { loc; arg } ->
      let arg = Option.map arg ~f:Expr.build_ast in
      return ~loc ~arg
end

module Decl = struct
  type t =
    | Type of {
        loc: Srcloc.t;
        ident: string;
        binding: Type.t;
      }
    | Let of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Fun of {
        loc: Srcloc.t;
        ident: string;
        params: string list;
        typ: Type.t;
        body: Stmt.t;
        pure: bool;
      }
    | Fun_expr of {
        loc: Srcloc.t;
        ident: string;
        params: string list;
        typ: Type.t;
        body: Expr.t;
      }
  [@@deriving sexp_of, variants]

  type builder = Env.t -> t * Env.t

  let type_ ~loc ~ident ~binding = fun env ->
    let binding = binding env in
    let env = Env.bind_type env ~ident ~typ:binding in
    type_ ~loc ~ident ~binding, env

  let let_ ~loc ~ident ~typ ~binding =
    if Fn.non Expr.is_pure binding
    then raise @@ Purity_error { loc = Expr.loc binding };
    Expr.typecheck ~typ binding;
    (let_) ~loc ~ident ~typ ~binding

  let let_ ~loc ~ident ~typ ~binding = fun env ->
    let typ = typ env in
    let binding = binding env in
    let env = Env.bind env ~ident ~typ ~pure:true in
    (let_) ~loc ~ident ~typ ~binding, env

  let fun_ ~loc ~ident ~params ~typ ~body ~pure =
    if pure && Fn.non Stmt.is_pure body
    then raise @@ Purity_error { loc };
    fun_ ~loc ~ident ~params ~typ ~body ~pure

  let fun_ ~loc ~ident ~params ~typ ~body ~pure = fun env ->
    let typ = typ env in
    let env = Env.bind env ~ident ~typ ~pure in
    let env' = Env.bind env ~ident:"return" ~typ:(Type.ret_exn typ) ~pure:true in
    let env' = List.fold2_exn params (Type.params_exn typ) ~init:env'
        ~f:(fun env ident typ -> Env.bind env ~ident ~typ ~pure:true) in
    let body, _ = body env' in
    fun_ ~loc ~ident ~params ~typ ~body ~pure, env

  let fun_expr ~loc ~ident ~params ~typ ~body =
    Expr.typecheck ~typ:(Type.ret_exn typ) body;
    if not @@ Expr.is_pure body
    then raise @@ Purity_error { loc = Expr.loc body };
    fun_expr ~loc ~ident ~params ~typ ~body

  let fun_expr ~loc ~ident ~params ~typ ~body = fun env ->
    let typ = typ env in
    let env = Env.bind env ~ident ~typ ~pure:true in
    let env' = List.fold2_exn params (Type.params_exn typ) ~init:env
        ~f:(fun env ident typ -> Env.bind env ~ident ~typ ~pure:true) in
    let body = body env' in
    fun_expr ~loc ~ident ~params ~typ ~body, env

  let build_ast (decl: Ast.Decl.t) =
    match decl with
    | Type { loc; ident; binding } ->
      let binding = Type.Builder.of_ast binding in
      (type_) ~loc ~ident ~binding
    | Let { loc; ident; typ; binding } ->
      (let_) ~loc ~ident ~typ:(Type.Builder.of_ast typ)
        ~binding:(Expr.build_ast binding)
    | Fun { loc; ident; params; ret_type; body; pure } ->
      let params, param_types = List.unzip params in
      let param_types = List.map param_types ~f:Type.Builder.of_ast in
      let ret_type = Type.Builder.of_ast ret_type in
      let typ = Type.Builder.fun_ ~params:param_types ~ret:ret_type in
      let body = Stmt.build_ast body in
      (fun_) ~loc ~ident ~params ~typ ~body ~pure
    | Fun_expr { loc; ident; params; ret_type; body } ->
      let params, param_types = List.unzip params in
      let param_types = List.map param_types ~f:Type.Builder.of_ast in
      let ret_type = Type.Builder.of_ast ret_type in
      let typ = Type.Builder.fun_ ~params:param_types ~ret:ret_type in
      let body = Expr.build_ast body in
      fun_expr ~loc ~ident ~params ~typ ~body
end

type t = Decl.t list
[@@deriving sexp_of]

type builder = Env.t -> Decl.t list * Env.t

let declare builder decl = fun env ->
  let decls, env = builder env in
  let decl, env = decl env in
  decls @ [decl], env

let build builder env = builder env

let rec build_ast (ast: Ast.t) builder =
  match ast with
  | [] -> build builder
  | decl :: ast ->
    let decl = Decl.build_ast decl in
    declare builder decl
    |> build_ast ast

let build_ast (ast: Ast.t) env = build_ast ast (fun env -> [], env) env
