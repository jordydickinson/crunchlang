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
    expected: [ `Type of Type.t | `Kind of Type.Kind.t ];
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

exception Invalid_abi of {
    loc: Srcloc.t;
    ident: string;
  }
[@@deriving sexp]

exception Coercion_error of {
    loc: Srcloc.t;
    dst_type: Type.t;
    src_type: Type.t;
  }
[@@deriving sexp]

module Env : sig
  type binding = {
    typ: Type.t;
    pure: bool;
  }

  type t

  val empty : t

  val prelude : t

  val bind : t -> ident:string -> typ:Type.t -> pure:bool -> t
  val lookup : t -> string -> binding option
  val typeof : t -> string -> Type.t option

  val bind_type : t -> ident:string -> typ:Type.t -> t
  val lookup_type : t -> string -> Type.t option
end = struct
  type binding = {
    typ: Type.t;
    pure: bool;
  }

  type t = {
    vars: binding String.Map.t;
    types: Type.t String.Map.t;
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

  let bind_type env ~ident ~typ =
    { env with types = Map.set env.types ~key:ident ~data:typ }

  let prelude =
    empty
    |> bind_type ~ident:"uint8" ~typ:Type.uint8
    |> bind_type ~ident:"int32" ~typ:Type.int32
    |> bind_type ~ident:"int64" ~typ:Type.int64
    |> bind_type ~ident:"bool" ~typ:Type.bool
    |> bind_type ~ident:"float64" ~typ:Type.float64
end

module Type = struct
  include Type

  let rec promote typ =
    match typ with
    | Void | Bool | Float64 | Pointer _ | Struct _ | Fun _ -> typ
    | Int { bitwidth; signed = _ } -> if bitwidth < 32 then int32 else typ
    | Array { elt; size } -> array ~elt:(promote elt) ~size

  module Builder : sig
    type typ = t
    type t

    val void : t
    val fun_ : params:t list -> ret:t -> t

    val build : t -> Env.t -> typ
    val of_ast : Ast.Type_expr.t -> t
  end = struct
    type typ = t
    type t = Env.t -> typ

    let build (builder: t) (env: Env.t) = builder env

    let void = fun _ -> void

    let struct_ fields = fun env ->
      let fields = List.map fields ~f:(fun (ident, typ) -> ident, typ env) in
      struct_ @@ fields

    let pointer arg = fun env ->
      pointer @@ arg env

    let array ~elt ~size = fun env ->
      array ~elt:(build elt env) ~size

    let fun_ ~params ~ret = fun env ->
      let ret = ret env in
      let params = List.map params ~f:(fun param -> param env) in
      fun_ ~params ~ret

    let rec of_ast (type_expr: Ast.Type_expr.t) =
      let of_name ~loc ~ident = fun env ->
        match Env.lookup_type env ident with
        | None -> raise @@ Unbound_type { loc; ident }
        | Some typ -> typ
      in
      match type_expr with
      | Name { loc; ident }  -> of_name ~loc ~ident
      | Pointer { loc = _; arg } -> pointer @@ of_ast arg
      | Array { loc = _; arg } -> array ~elt:(of_ast arg) ~size:0
      | Struct { loc = _; fields } -> struct_ @@ List.map fields
          ~f:(fun (ident, typ) -> ident, of_ast typ)
  end
end

module Expr = struct
  module Bop = Ast.Expr.Bop

  type t =
    | Int of {
        loc: Srcloc.t;
        value: int64;
        typ: Type.t;
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
    | Cast of {
        loc: Srcloc.t option [@sexp.option];
        typ: Type.t;
        arg: t;
      }
    | Deref of {
        loc: Srcloc.t;
        arg: t;
        typ: Type.t;
        pure: bool;
      }
    | Addr_of of {
        loc: Srcloc.t;
        arg: t;
        typ: Type.t;
      }
    | Array of {
        loc: Srcloc.t;
        elts: t array;
        typ: Type.t;
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

  let rec loc = function
    | Int { loc; _ }
    | Bool { loc; _ }
    | Float { loc; _ }
    | Name { loc; _ }
    | Deref { loc; _ }
    | Addr_of { loc; _ }
    | Array { loc; _ }
    | Binop { loc; _ }
    | Call { loc; _ }
    | Let_in { loc; _ }
    | Cast { loc = Some loc; _ } -> loc
    | Cast { arg; _ } -> loc arg

  let rec typ = function
    | Bool _ -> Type.bool
    | Float _ -> Type.float64
    | Int { typ; _ }
    | Name { typ; _ }
    | Cast { typ; _ }
    | Deref { typ; _ }
    | Addr_of { typ; _ }
    | Array { typ; _ } -> typ
    | Binop { lhs; _ } -> typ lhs
    | Call { callee; _ } -> Type.ret_exn @@ typ callee
    | Let_in { body; _ } -> typ body

  let rec is_promotable = function
    | Int _ -> true
    | Binop { lhs; rhs; _ } -> is_promotable lhs && is_promotable rhs
    | Array { elts; _ } -> Array.for_all elts ~f:is_promotable
    | Bool _ | Float _ | Name _ | Cast _
    | Deref _ | Addr_of _ | Call _ | Let_in _  -> false

  let rec impurities = function
    | Int _ | Bool _ | Float _
    | Name { pure = true; _ }
    | Deref { pure = true; _ } -> String.Set.empty
    | Name { ident; _ } -> String.Set.singleton ident
    | Cast { arg; _ }
    | Deref { arg; _ }
    | Addr_of { arg; _ } -> impurities arg
    | Array { elts; _ } ->
      String.Set.union_list (Array.map elts ~f:impurities |> Array.to_list)
    | Binop { lhs; rhs; _ } -> Set.union (impurities lhs) (impurities rhs)
    | Call { callee; args; _ } ->
      String.Set.union_list (impurities callee :: List.map ~f:impurities args)
    | Let_in { body; _ } -> impurities body

  let is_pure expr = Set.is_empty @@ impurities expr

  let rec is_lvalue = function
    | Name { pure = false; _ }
    | Deref { pure = false; _ } -> true
    | Cast { arg; _ } -> is_lvalue arg
    | Name _ | Deref _ -> false
    | Int _ | Bool _ | Float _
    | Addr_of _ | Array _ | Binop _ | Call _
    | Let_in _ -> false

  let check_lvalue expr =
    if not @@ is_lvalue expr
    then raise @@ Not_assignable { loc = loc expr }

  let typecheck ~typ:typ' expr =
    if not @@ Type.equal typ' @@ typ expr
    then raise @@ Type_error { loc = loc expr; expected = `Type typ'; got = typ expr }

  let typecheck_kind ~kind expr =
    if not @@ Type.is_kind (typ expr) kind
    then raise @@ Type_error { loc = loc expr; expected = `Kind kind; got = typ expr }

  let rec int ?typ ~loc value =
    let typ = Option.value typ
        ~default:(if Int64.is_non_negative value
                  && Int64.O.(value < Int64.of_int 256)
                  then Type.uint8
                  else Type.int64) in
    Int { loc; typ; value }

  and cast ?loc ~typ arg =
    Cast { loc; typ; arg }

  and coerce ~typ:(typ': Type.t) arg =
    match arg, Type.union typ' @@ typ arg with
    | _, None ->
      raise @@ Coercion_error { loc = loc arg; src_type = typ arg; dst_type = typ' }
    | _ when Type.equal typ' @@ typ arg -> arg
    | Int { loc; value; typ = _ }, _ -> int ~loc ~typ:typ' value
    | Binop { loc; op; lhs; rhs }, _ ->
      binop ~loc ~op ~lhs:(coerce ~typ:typ' lhs) ~rhs:(coerce ~typ:typ' rhs)
    | Array { loc; typ = _; elts }, Some typ' ->
      array ~loc ~typ:typ' elts
    | _ -> cast ~typ:typ' arg

  and array ?typ:(typ': Type.t option) ~loc elts =
    let elt_type = if Array.length elts = 0 then Type.void else typ elts.(0) in
    let typ = Option.value typ'
        ~default:(Type.array ~elt:elt_type ~size:(Array.length elts)) in
    match typ with
    | Array { elt; size } ->
      assert (size = Array.length elts);
      let elts = Array.map elts ~f:(coerce ~typ:elt) in
      Array { loc; typ; elts }
    | _ -> assert false

  and binop ~loc ~op:Bop.Add ~lhs ~rhs =
    typecheck_kind ~kind:Type.Kind.numeric lhs;
    typecheck_kind ~kind:Type.Kind.numeric rhs;
    let typ = Type.union (typ lhs) (typ rhs) |> Option.value_exn in
    Binop { loc; op = Bop.Add; lhs = coerce ~typ lhs; rhs = coerce ~typ rhs }

  module Builder : sig
    type expr = t
    type t

    val build : t -> Env.t -> expr
    val of_ast : Ast.Expr.t -> t
  end = struct
    type expr = t
    type t = Env.t -> expr

    let build (builder: t) (env: Env.t) : expr = builder env

    let int ~loc ~value = fun _ -> int ~loc value

    let bool ~loc ~value = fun _ -> bool ~loc ~value

    let float ~loc ~value = fun _ -> float ~loc ~value

    let name ~loc ~ident = fun env ->
      match Env.lookup env ident with
      | Some { typ; pure } -> name ~loc ~ident ~typ ~pure
      | None -> raise @@ Unbound_identifier { loc; ident }

    let cast ~loc ~typ arg = fun env ->
      cast ~loc ~typ:(Type.Builder.build typ env) (build arg env)

    let deref ~loc ~arg =
      typecheck_kind ~kind:Type.Kind.pointer arg;
      let typ = Type.deref_exn @@ typ arg in
      let pure = is_pure arg in
      deref ~loc ~arg ~typ ~pure

    let deref ~loc ~arg = fun env ->
      let arg = arg env in
      deref ~loc ~arg

    let addr_of ~loc ~arg =
      check_lvalue arg;
      let typ = Type.pointer @@ typ arg in
      addr_of ~loc ~arg ~typ

    let addr_of ~loc ~arg = fun env ->
      let arg = arg env in
      addr_of ~loc ~arg

    let array ~loc ~elts = fun env ->
      let elts = Array.map elts ~f:(fun elt -> build elt env) in
      array ~loc elts

    let binop ~loc ~op ~lhs ~rhs = fun env ->
      binop ~loc ~op ~lhs:(lhs env) ~rhs:(rhs env)

    let call ~loc ~callee ~args =
      let args =
        match List.map2 (Type.params_exn @@ typ callee) args
                ~f:(fun typ arg -> coerce arg ~typ)
        with
        | Ok args -> args
        | Unequal_lengths ->
          raise @@ Arity_mismatch {
            loc;
            expected = List.length @@ Type.params_exn @@ typ callee;
            got = List.length args;
          } in
      call ~loc ~callee ~args

    let call ~loc ~callee ~args = fun env ->
      call ~loc ~callee:(callee env) ~args:(List.map args ~f:(fun arg -> arg env))

    let let_in ?binding_type ~loc:loc' ~ident ~binding ~body =
      let binding = match binding_type with
        | None ->
          if is_promotable binding
          then coerce ~typ:(Type.promote @@ typ binding) binding
          else binding
        | Some typ -> coerce ~typ binding in
      if not @@ is_pure binding
      then raise @@ Purity_error { loc = loc binding };
      (let_in) ~loc:loc' ~ident ~binding ~body

    let let_in ?binding_type ~loc ~ident ~binding ~body = fun env ->
      let binding_type = Option.map binding_type ~f:(fun typ -> Type.Builder.build typ env) in
      let binding = binding env in
      let env = Env.bind env ~ident ~typ:(typ binding) ~pure:true in
      let body = body env in
      (let_in) ?binding_type ~loc ~ident ~binding ~body

    let rec of_ast (expr: Ast.Expr.t) : t =
      match expr with
      | Int { loc; value } -> int ~loc ~value
      | Bool { loc; value } -> bool ~loc ~value
      | Float { loc; value } -> float ~loc ~value
      | Name { loc; ident } -> name ~loc ~ident
      | Array { loc; elts } -> array ~loc ~elts:(Array.map elts ~f:of_ast)
      | Cast { loc; arg; typ } -> cast ~loc (of_ast arg) ~typ:(Type.Builder.of_ast typ)
      | Deref { loc; arg } -> deref ~loc ~arg:(of_ast arg)
      | Addr_of { loc; arg } -> addr_of ~loc ~arg:(of_ast arg)
      | Binop { loc; op; lhs; rhs } ->
        let lhs = of_ast lhs in
        let rhs = of_ast rhs in
        binop ~loc ~op ~lhs ~rhs
      | Call { loc; callee; args } ->
        let callee = of_ast callee in
        let args = List.map args ~f:of_ast in
        call ~loc ~callee ~args
      | Let_in { loc; ident; typ; binding; body } ->
        let binding_type = Option.map ~f:Type.Builder.of_ast typ in
        let binding = of_ast binding in
        let body = of_ast body in
        (let_in) ~loc ~ident ?binding_type ~binding ~body
  end
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

  module Builder : sig
    type stmt = t
    type t

    val of_ast : Ast.Stmt.t -> t
    val build : t -> Env.t -> stmt * Env.t
  end = struct
    type stmt = t
    type t = Env.t -> stmt * Env.t

    let build (builder: t) (env: Env.t): stmt * Env.t = builder env

    let expr expr' = fun env -> expr @@ Expr.Builder.build expr' env, env

    let block stmts = fun env ->
      let rec block' ~accum env = function
        | [] -> List.rev accum
        | stmt :: stmts ->
          let stmt, env = stmt env in
          block' env stmts ~accum:(stmt :: accum)
      in
      block @@ block' env stmts ~accum:[], env

    let assign ~loc ~src ~dst =
      Expr.check_lvalue dst;
      assign ~loc ~dst ~src:(Expr.coerce ~typ:(Expr.typ dst) src)

    let assign ~loc ~dst ~src = fun env ->
      assign ~loc ~dst:(Expr.Builder.build dst env) ~src:(Expr.Builder.build src env), env

    let let_ ~loc ~typ ~ident ~binding =
      if Fn.non Expr.is_pure binding
      then raise @@ Purity_error { loc = Expr.loc binding };
      (let_) ~loc ~ident ~typ ~binding:(Expr.coerce ~typ binding)

    let let_ ~loc ~typ ~ident ~binding = fun env ->
      let typ = Option.map typ ~f:(fun typ -> Type.Builder.build typ env) in
      let binding = Expr.Builder.build binding env in
      let binding = match typ with
        | None ->
          if Expr.is_promotable binding
          then Expr.coerce ~typ:(Type.promote @@ Expr.typ binding) binding
          else binding
        | Some typ -> Expr.coerce ~typ binding in
      let typ = Expr.typ binding in
      let env = Env.bind env ~ident ~typ:(Expr.typ binding) ~pure:true in
      (let_) ~loc ~typ ~ident ~binding, env

    let var ~loc ~typ ~ident ~binding = fun env ->
      let typ = Option.map typ ~f:(fun typ -> Type.Builder.build typ env) in
      let binding = Expr.Builder.build binding env in
      let binding = match typ with
        | None ->
          if Expr.is_promotable binding
          then Expr.coerce ~typ:(Type.promote @@ Expr.typ binding) binding
          else binding
        | Some typ -> Expr.coerce ~typ binding in
      let typ = Expr.typ binding in
      let env = Env.bind env ~ident ~typ:(Expr.typ binding) ~pure:false in
      var ~loc ~typ ~ident ~binding, env

    let if_ ~loc ~cond ~iftrue ~iffalse = fun env ->
      let cond = Expr.Builder.build cond env in
      let iftrue, _ = iftrue env in
      let iffalse = match iffalse with
        | Some iffalse -> Some (fst @@ iffalse env)
        | None -> None in
      if_ ~loc ~cond ~iftrue ~iffalse, env

    let return ~loc ~arg = fun env ->
      let ret_type = Env.typeof env "return" |> Option.value_exn in
      let arg = match arg with
        | Some arg ->
          let arg = Expr.Builder.build arg env in
          Some (Expr.coerce ~typ:ret_type arg)
        | None ->
          if not @@ Type.equal ret_type Type.void
          then raise @@ Type_error { loc; expected = `Type ret_type; got = Type.void };
          None in
      return ~loc ~arg, env

    let rec of_ast (stmt: Ast.Stmt.t) =
      match stmt with
      | Expr expr' -> expr @@ Expr.Builder.of_ast expr'
      | Block stmts -> block @@ List.map stmts ~f:of_ast
      | Assign { loc; dst; src } ->
        let dst = Expr.Builder.of_ast dst in
        let src = Expr.Builder.of_ast src in
        assign ~loc ~dst ~src
      | Let { loc; ident; typ; binding } ->
        let typ = Option.map ~f:Type.Builder.of_ast typ in
        let binding = Expr.Builder.of_ast binding in
        (let_) ~loc ~ident ~typ ~binding
      | Var { loc; ident; typ; binding } ->
        let typ = Option.map ~f:Type.Builder.of_ast typ in
        let binding = Expr.Builder.of_ast binding in
        var ~loc ~ident ~typ ~binding
      | If { loc; cond; iftrue; iffalse } ->
        let cond = Expr.Builder.of_ast cond in
        let iftrue = of_ast iftrue in
        let iffalse = Option.map iffalse ~f:of_ast in
        if_ ~loc ~cond ~iftrue ~iffalse
      | Return { loc; arg } ->
        let arg = Option.map arg ~f:Expr.Builder.of_ast in
        return ~loc ~arg
  end
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
    | Fun_extern of {
        loc: Srcloc.t;
        ident: string;
        params: string list;
        typ: Type.t;
        pure: bool [@sexp.bool];
        extern_abi: string;
        extern_ident: string;
      }
  [@@deriving sexp_of, variants]

  module Builder : sig
    type decl = t
    type t

    val of_ast : Ast.Decl.t -> t
    val build : t -> Env.t -> decl * Env.t
  end = struct
    type decl = t
    type t = Env.t -> decl * Env.t

    let build (builder: t) (env: Env.t) : decl * Env.t = builder env

    let type_ ~loc ~ident ~binding = fun env ->
      let binding = Type.Builder.build binding env in
      let env = Env.bind_type env ~ident ~typ:binding in
      type_ ~loc ~ident ~binding, env

    let let_ ~loc ~ident ~typ ~binding =
      if Fn.non Expr.is_pure binding
      then raise @@ Purity_error { loc = Expr.loc binding };
      (let_) ~loc ~ident ~typ ~binding:(Expr.coerce ~typ binding)

    let let_ ~loc ~ident ~typ ~binding = fun env ->
      let typ = Type.Builder.build typ env in
      let binding = Expr.Builder.build binding env in
      let env = Env.bind env ~ident ~typ ~pure:true in
      (let_) ~loc ~ident ~typ ~binding, env

    let fun_ ~loc ~ident ~params ~typ ~body ~pure =
      if pure && Fn.non Stmt.is_pure body
      then raise @@ Purity_error { loc };
      fun_ ~loc ~ident ~params ~typ ~body ~pure

    let fun_ ~loc ~ident ~params ~typ ~body ~pure = fun env ->
      let typ = Type.Builder.build typ env in
      let env = Env.bind env ~ident ~typ ~pure in
      let env' = Env.bind env ~ident:"return" ~typ:(Type.ret_exn typ) ~pure:true in
      let env' = List.fold2_exn params (Type.params_exn typ) ~init:env'
          ~f:(fun env ident typ -> Env.bind env ~ident ~typ ~pure:true) in
      let body, _ = Stmt.Builder.build body env' in
      fun_ ~loc ~ident ~params ~typ ~body ~pure, env

    let fun_expr ~loc ~ident ~params ~typ ~body =
      Expr.typecheck ~typ:(Type.ret_exn typ) body;
      if not @@ Expr.is_pure body
      then raise @@ Purity_error { loc = Expr.loc body };
      fun_expr ~loc ~ident ~params ~typ ~body

    let fun_expr ~loc ~ident ~params ~typ ~body = fun env ->
      let typ = Type.Builder.build typ env in
      let env = Env.bind env ~ident ~typ ~pure:true in
      let env' = List.fold2_exn params (Type.params_exn typ) ~init:env
          ~f:(fun env ident typ -> Env.bind env ~ident ~typ ~pure:true) in
      let body = Expr.Builder.build body env' in
      fun_expr ~loc ~ident ~params ~typ ~body, env

    let fun_extern ~loc ~ident ~params ~typ ~extern_abi ~extern_ident ~pure = fun env ->
      let extern_abi = match extern_abi with
        | "C" -> "C"
        | _ -> raise @@ Invalid_abi { loc; ident } in
      let typ = Type.Builder.build typ env in
      let env = Env.bind env ~ident ~typ ~pure in
      fun_extern ~loc ~ident ~params ~typ ~extern_abi ~extern_ident ~pure,
      env

    let of_ast (decl: Ast.Decl.t) =
      match decl with
      | Type { loc; ident; binding } ->
        let binding = Type.Builder.of_ast binding in
        (type_) ~loc ~ident ~binding
      | Let { loc; ident; typ; binding } ->
        (let_) ~loc ~ident ~typ:(Type.Builder.of_ast typ)
          ~binding:(Expr.Builder.of_ast binding)
      | Fun { loc; ident; params; ret_type; body; pure } ->
        let params, param_types = List.unzip params in
        let param_types = List.map param_types ~f:Type.Builder.of_ast in
        let ret_type = Option.value_map ret_type ~f:Type.Builder.of_ast
            ~default:Type.Builder.void in
        let typ = Type.Builder.fun_ ~params:param_types ~ret:ret_type in
        let body = Stmt.Builder.of_ast body in
        (fun_) ~loc ~ident ~params ~typ ~body ~pure
      | Fun_expr { loc; ident; params; ret_type; body } ->
        let params, param_types = List.unzip params in
        let param_types = List.map param_types ~f:Type.Builder.of_ast in
        let ret_type = Type.Builder.of_ast ret_type in
        let typ = Type.Builder.fun_ ~params:param_types ~ret:ret_type in
        let body = Expr.Builder.of_ast body in
        fun_expr ~loc ~ident ~params ~typ ~body
      | Fun_extern { loc; ident; params; ret_type; extern_abi; extern_ident; pure } ->
        let params, param_types = List.unzip params in
        let param_types = List.map param_types ~f:Type.Builder.of_ast in
        let ret_type = Option.value_map ret_type ~f:Type.Builder.of_ast
            ~default:Type.Builder.void in
        let typ = Type.Builder.fun_ ~params:param_types ~ret:ret_type in
        fun_extern ~loc ~ident ~params ~typ ~extern_abi ~extern_ident ~pure
  end

  type builder = Builder.t
end

type t = Decl.t list
[@@deriving sexp_of]

module Builder : sig
  type semantic = t
  type t

  val of_ast : Ast.t -> t
  val build : t -> Env.t -> semantic * Env.t
end = struct
  type semantic = t
  type t = Env.t -> semantic * Env.t

  let build (builder: t) (env: Env.t) : semantic * Env.t = builder env

  let declare (builder: t) (decl: Decl.builder) = fun env ->
    let decl, env = Decl.Builder.build decl env in
    let decls, env = builder env in
    decl :: decls, env

  let rec of_ast (ast: Ast.t) : t =
    match ast with
    | [] -> fun env -> [], env
    | decl :: ast ->
      let decl = Decl.Builder.of_ast decl in
      declare (of_ast ast) decl

end

let build_ast (ast: Ast.t) = Builder.build (Builder.of_ast ast)
