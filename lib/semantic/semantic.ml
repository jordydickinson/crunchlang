exception Unbound_identifier of {
    loc: Srcloc.t;
    ident: string;
  }

exception Type_error of {
    loc: Srcloc.t;
    expected: Type.t list;
    got: Type.t;
  }

exception Arity_mismatch of {
    loc: Srcloc.t;
    expected: int;
    got: int;
  }

exception Purity_error of {
    loc: Srcloc.t
  }

module Env : sig
  type binding = {
    typ: Type.t;
    pure: bool;
  }

  type t

  val empty : t

  val lookup : t -> string -> binding option

  val typeof : t -> string -> Type.t option

  val bind : t -> ident:string -> typ:Type.t -> pure:bool -> t
end = struct
  type binding = {
    typ: Type.t;
    pure: bool;
  }

  type t = binding String.Map.t

  let empty = String.Map.empty

  let lookup env ident = Map.find env ident

  let typeof env ident =
    Option.map (lookup env ident) ~f:(function { typ; _ } -> typ)

  let bind env ~ident ~typ ~pure =
    Map.set env ~key:ident ~data:{ typ; pure }
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
    | Binop { loc; _ }
    | Call { loc; _ }
    | Let_in { loc; _ } -> loc

  let rec typ = function
    | Int _ -> Type.int64
    | Bool _ -> Type.bool
    | Float _ -> Type.float
    | Name { typ; _ } -> typ
    | Binop { lhs; _ } -> typ lhs
    | Call { callee; _ } -> Type.ret_exn @@ typ callee
    | Let_in { body; _ } -> typ body

  let rec impurities = function
    | Int _ | Bool _ | Float _ -> String.Set.empty
    | Name { ident; pure; _ } ->
      if pure then String.Set.empty else String.Set.singleton ident
    | Binop { lhs; rhs; _ } -> Set.union (impurities lhs) (impurities rhs)
    | Call { callee; args; _ } ->
      String.Set.union_list (impurities callee :: List.map ~f:impurities args)
    | Let_in { body; _ } -> impurities body

  let is_pure expr = Set.is_empty @@ impurities expr

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
    | Binop { loc; op; lhs; rhs } ->
      let lhs = build_ast lhs in
      let rhs = build_ast rhs in
      binop ~loc ~op ~lhs ~rhs
    | Call { loc; callee; args } ->
      let callee = build_ast callee in
      let args = List.map args ~f:build_ast in
      call ~loc ~callee ~args
    | Let_in { loc; ident; typ; binding; body } ->
      let binding_type = Option.map ~f:Type.of_type_expr typ in
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
    let binding = binding env in
    let typ = Option.value typ ~default:(Expr.typ binding) in
    let env = Env.bind env ~ident ~typ:(Expr.typ binding) ~pure:true in
    (let_) ~loc ~typ ~ident ~binding, env

  let var ~loc ~typ ~ident ~binding =
    Expr.typecheck ~typ binding;
    var ~loc ~ident ~typ ~binding

  let var ~loc ~typ ~ident ~binding = fun env ->
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
      let typ = Option.map ~f:Type.of_type_expr typ in
      let binding = Expr.build_ast binding in
      (let_) ~loc ~ident ~typ ~binding
    | Var { loc; ident; typ; binding } ->
      let typ = Option.map ~f:Type.of_type_expr typ in
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

  let typ = function
    | Let { typ; _ }
    | Fun { typ; _ }
    | Fun_expr { typ; _ } -> typ

  let let_ ~loc ~ident ~typ ~binding =
    if Fn.non Expr.is_pure binding
    then raise @@ Purity_error { loc = Expr.loc binding };
    Expr.typecheck ~typ binding;
    (let_) ~loc ~ident ~typ ~binding

  let let_ ~loc ~ident ~typ ~binding = fun env ->
    let binding = binding env in
    let env = Env.bind env ~ident ~typ ~pure:true in
    (let_) ~loc ~ident ~typ ~binding, env

  let fun_ ~loc ~ident ~params ~typ ~body ~pure =
    if pure && Fn.non Stmt.is_pure body
    then raise @@ Purity_error { loc };
    fun_ ~loc ~ident ~params ~typ ~body ~pure

  let fun_ ~loc ~ident ~params ~typ ~body ~pure = fun env ->
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
    let env = Env.bind env ~ident ~typ ~pure:true in
    let env' = List.fold2_exn params (Type.params_exn typ) ~init:env
        ~f:(fun env ident typ -> Env.bind env ~ident ~typ ~pure:true) in
    let body = body env' in
    fun_expr ~loc ~ident ~params ~typ ~body, env

  let build_ast (decl: Ast.Decl.t) =
    match decl with
    | Type _ -> assert false
    | Let { loc; ident; typ; binding } ->
      (let_) ~loc ~ident ~typ:(Type.of_type_expr typ)
        ~binding:(Expr.build_ast binding)
    | Fun { loc; ident; params; ret_type; body; pure } ->
      let params, param_types = List.unzip params in
      let param_types = List.map param_types ~f:Type.of_type_expr in
      let ret_type = Type.of_type_expr ret_type in
      let typ = Type.fun_ ~params:param_types ~ret:ret_type in
      let body = Stmt.build_ast body in
      (fun_) ~loc ~ident ~params ~typ ~body ~pure
    | Fun_expr { loc; ident; params; ret_type; body } ->
      let params, param_types = List.unzip params in
      let param_types = List.map param_types ~f:Type.of_type_expr in
      let ret_type = Type.of_type_expr ret_type in
      let typ = Type.fun_ ~params:param_types ~ret:ret_type in
      let body = Expr.build_ast body in
      fun_expr ~loc ~ident ~params ~typ ~body
end

type t = Decl.t list

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
