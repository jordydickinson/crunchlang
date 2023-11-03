[@@@warning "-16"]

exception Unbound_identifier of {
    loc: Srcloc.t option [@sexp.option];
    ident: string;
  }
[@@deriving sexp]

exception Unbound_type of {
    loc: Srcloc.t option [@sexp.option];
    ident: string;
  }
[@@deriving sexp]

exception Type_error of {
    loc: Srcloc.t option [@sexp.option];
    expected: [ `Type of Type.t | `Kind of Type.Kind.t ];
    got: Type.t;
  }
[@@deriving sexp]

exception Arity_mismatch of {
    loc: Srcloc.t option [@sexp.option];
    expected: int;
    got: int;
  }
[@@deriving sexp]

exception Purity_error of {
    loc: Srcloc.t
  }
[@@deriving sexp]

exception Not_assignable of {
    loc: Srcloc.t option [@sexp.option];
  }
[@@deriving sexp]

exception Invalid_abi of {
    loc: Srcloc.t option [@sexp.option];
    ident: string;
  }
[@@deriving sexp]

exception Coercion_error of {
    loc: Srcloc.t option [@sexp.option];
    dst_type: Type.t;
    src_type: Type.t;
  }
[@@deriving sexp]

let type_error ~(expected: Type.t) ~(got: Type.t) : 'a =
  ok_exn begin
    Or_error.error_string "type error"
    |> fun e -> Or_error.tag_arg e "expected" expected [%sexp_of: Type.t]
    |> fun e -> Or_error.tag_arg e "got" got [%sexp_of: Type.t]
  end

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
    |> bind_type ~ident:"float32" ~typ:Type.float32
    |> bind_type ~ident:"float64" ~typ:Type.float64
end

module Type = struct
  include Type

  let rec promote typ =
    match typ with
    | Void | Bool | Float32 | Float64 | Reference _ | Struct _ | Fun _ -> typ
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

    let reference arg = fun env ->
      reference @@ arg env

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
      | Reference { loc = _; arg } -> reference @@ of_ast arg
      | Array { loc = _; arg } -> array ~elt:(of_ast arg) ~size:0
      | Struct { loc = _; fields } -> struct_ @@ List.map fields
          ~f:(fun (ident, typ) -> ident, of_ast typ)
  end
end

module Expr = struct
  module Bop = Ast.Expr.Bop

  type t =
    | Int of {
        loc: Srcloc.t option [@sexp.option];
        value: int64;
        typ: Type.t;
      }
    | Bool of {
        loc: Srcloc.t option [@sexp.option];
        value: bool;
      }
    | Float of {
        loc: Srcloc.t option [@sexp.option];
        value: float;
        typ: Type.t;
      }
    | Name of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type.t;
        pure: bool;
      }
    | Cast of {
        loc: Srcloc.t option [@sexp.option];
        typ: Type.t;
        arg: t;
      }
    | Deref of t
    | Addr_of of t
    | Array of {
        loc: Srcloc.t option [@sexp.option];
        elts: t array;
        typ: Type.t;
      }
    | Subscript of {
        loc: Srcloc.t option [@sexp.option];
        arg: t;
        idx: t;
      }
    | Binop of {
        loc: Srcloc.t option [@sexp.option];
        op: Bop.t;
        lhs: t;
        rhs: t;
      }
    | Call of {
        loc: Srcloc.t option [@sexp.option];
        callee: t;
        args: t list;
      }
    | Let_in of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        binding: t;
        body: t;
      }
  [@@deriving sexp_of]

  let rec loc = function
    | Deref arg
    | Addr_of arg -> loc arg
    | Int { loc; _ }
    | Bool { loc; _ }
    | Float { loc; _ }
    | Name { loc; _ }
    | Array { loc; _ }
    | Subscript { loc; _ }
    | Binop { loc; _ }
    | Call { loc; _ }
    | Let_in { loc; _ }
    | Cast { loc; _ } -> loc

  let loc_exn expr = Option.value_exn (loc expr)

  let rec typ = function
    | Bool _ -> Type.bool
    | Deref arg -> Type.deref_exn @@ typ arg
    | Addr_of arg -> Type.reference @@ typ arg
    | Int { typ; _ }
    | Float { typ; _ }
    | Name { typ; _ }
    | Cast { typ; _ }
    | Array { typ; _ } -> typ
    | Subscript { arg; _ } -> Type.elt_exn @@ typ arg
    | Binop { lhs; _ } -> typ lhs
    | Call { callee; _ } -> Type.ret_exn @@ typ callee
    | Let_in { body; _ } -> typ body

  let rec is_promotable = function
    | Int _ -> true
    | Binop { lhs; rhs; _ } -> is_promotable lhs && is_promotable rhs
    | Array { elts; _ } -> Array.for_all elts ~f:is_promotable
    | Bool _ | Float _ | Name _ | Cast _ | Subscript _
    | Deref _ | Addr_of _ | Call _ | Let_in _  -> false

  let rec impurities = function
    | Int _ | Bool _ | Float _
    | Name { pure = true; _ } -> String.Set.empty
    | Name { ident; _ } -> String.Set.singleton ident
    | Cast { arg; _ } | Deref arg | Addr_of arg -> impurities arg
    | Array { elts; _ } ->
      String.Set.union_list (Array.map elts ~f:impurities |> Array.to_list)
    | Subscript { arg; idx; _ } -> Set.union (impurities arg) (impurities idx)
    | Binop { lhs; rhs; _ } -> Set.union (impurities lhs) (impurities rhs)
    | Call { callee; args; _ } ->
      String.Set.union_list (impurities callee :: List.map ~f:impurities args)
    | Let_in { body; _ } -> impurities body

  let is_pure expr = Set.is_empty @@ impurities expr

  let is_literal = function
    | Int _ | Bool _ | Float _ | Array _ -> true
    | _ -> false

  let rec is_lvalue = function
    | Name { pure = false; _ }
      -> true
    | Deref arg | Addr_of arg
    | Subscript { arg; _ }
    | Cast { arg; _ }
      -> is_lvalue arg
    | Int _ | Bool _ | Float _ | Name _
    | Array _ | Binop _ | Call _
    | Let_in _
      -> false

  let check_lvalue expr =
    if not @@ is_lvalue expr
    then raise @@ Not_assignable { loc = loc expr }

  let typecheck ~typ:typ' expr =
    if not @@ Type.equal typ' @@ typ expr
    then raise @@ Type_error { loc = loc expr; expected = `Type typ'; got = typ expr }

  let typecheck_kind ~kind expr =
    if not @@ Type.is_kind (typ expr) kind
    then raise @@ Type_error { loc = loc expr; expected = `Kind kind; got = typ expr }

  let bool ?loc value = Bool { loc; value }
  let name ?loc ~typ ~pure ident = Name { loc; typ; pure; ident }

  let rec int ?loc ?typ value =
    let typ = Option.value typ
        ~default:(if Int64.is_non_negative value
                  && Int64.O.(value < Int64.of_int 256)
                  then Type.uint8
                  else Type.int64) in
    Int { loc; typ; value }

  and float ?loc ?typ value =
    let typ = Option.value typ ~default:Type.float64 in
    Float { loc; typ; value }

  (** [cast ?loc ~typ arg] is [arg as typ]. In addition to conversions done by
      [coerce ~typ arg], this can do certain additional conversions which are
      not wise to do implicitly.

      Types for which explicit conversions can be performed include all those
      which can be converted implicitly, and additionally "unsafe" numeric
      conversions (e.g., downcasts).

      Some types cannot be converted by [cast]. For example, [t[]] cannot be
      converted to [t&] or vice-versa. *)
  and cast ?(loc: Srcloc.t option) ~typ:(typ': Type.t) (arg: t) : t =
    let cast_numeric ?loc ~typ:(typ': Type.t) (arg: t) =
      match typ', arg with
      (* These can be done inline. *)
      | Int _, Int { loc; value; typ = _ } -> int ?loc ~typ:typ' value
      | Int _, Float { loc; value; typ = _ } -> int ?loc ~typ:typ' (Int64.of_float value)
      | (Float32 | Float64), Int { loc; value; typ = _ } -> float ?loc ~typ:typ' (Float.of_int64 value)
      | (Float32 | Float64), Float { loc; value; typ = _ } -> float ?loc ~typ:typ' value
      (* Everything else will have to wait. *)
      | _ -> Cast { loc; typ = typ'; arg }
    in match typ', typ arg with
    | Void, _ | _, Void ->
      (* These should never happen. *)
      failwith "internal error: cast to or from void"
    | Reference _, _ | _, Reference _
    | Array _, _ ->
      (* [coerce] handles all valid cases of these. *)
      coerce ~typ:typ' arg
    | Int _, _ | Float32, _ | Float64, _ -> cast_numeric ?loc ~typ:typ' arg
    | Struct _, _ | _, Struct _
    | Fun _, _ | _, Fun _
    | Bool, _ ->
      (* There are no valid conversions between these except implicit coercions
         to/from reference types, which were already handled. *)
      type_error ~expected:typ' ~got:(typ arg)

  (** [coerce ~typ arg] is [arg] coerced to type [typ], if such a conversion can
      be done implicitly. If the conversion cannot be done implicitly, an error
      is returned.

      Types for which implicit conversions can be performed include numeric
      types (when the conversion is "safe", i.e., an upcast), coercions to
      types which differ only in level of indirection (e.g., [t] to [t&]).

      In addition, literal expressions have much more flexible coercion rules,
      based on their particular values, since this is known at compile-time. *)
  and coerce ~typ:(typ': Type.t) (arg: t) : t =
    let coerce_literal ~typ:(typ': Type.t) arg =
      match arg, Type.union typ' @@ typ arg with
      | _, None -> type_error ~expected:typ' ~got:(typ arg)
      | Int { loc; value; typ = _ }, _ -> int ?loc ~typ:typ' value
      | Binop { loc; op; lhs; rhs }, _ ->
        binop ~loc ~op ~lhs:(coerce ~typ:typ' lhs) ~rhs:(coerce ~typ:typ' rhs)
      | Array { loc; elts; typ = _ }, Some (Array { elt; _ }) ->
        array ?loc (Array.map elts ~f:(coerce ~typ:elt))
      | _, _ -> invalid_arg "coerce_literal called with non-literal argument"
    in let coerce_numeric ~typ:(typ': Type.t) arg =
      match Type.union typ' @@ typ arg with
      | None -> type_error ~expected:typ' ~got:(typ arg)
      | Some typ' -> cast ~typ:typ' arg
    in let coerce_reference ~typ:(typ': Type.t) arg =
      match typ', typ arg with
      | Reference typ', Reference _ -> addr_of @@ coerce ~typ:typ' (deref arg)
      | Reference _, _ -> coerce ~typ:typ' (addr_of arg)
      | _, Reference _ -> coerce ~typ:typ' (to_value arg)
      | typ', typ when Type.equal typ' typ -> arg
      | typ', typ -> type_error ~expected:typ' ~got:typ
    in if Type.equal typ' (typ arg) then arg
    else if Type.is_reference typ' then coerce_reference ~typ:typ' arg
    else if is_literal arg then coerce_literal ~typ:typ' arg
    else if Type.is_numeric typ' then coerce_numeric ~typ:typ' arg
    else type_error ~expected:typ' ~got:(typ arg)

  (** [to_value arg] is [arg] dereferenced as many times as necessary to obtain
      a non-reference type. In that case that [arg] is not of a reference type,
      this is simply the identity function. *)
  and to_value arg =
    if Type.is_reference @@ typ arg
    then to_value @@ deref arg
    else arg

  (** [deref arg] is [arg] dereferenced once. [arg] must be of a reference type. *)
  and deref arg =
    match arg with
    | Addr_of arg -> arg
    | arg -> typecheck_kind ~kind:Type.Kind.reference arg; Deref arg

  (** [addr_of arg] is the address of [arg]. *)
  and addr_of arg =
    match arg with
    | Deref arg -> arg
    | arg -> Addr_of arg

  and array ?(loc: Srcloc.t option) ?typ:(typ': Type.t option) elts =
    let elt_type = if Array.length elts = 0 then Type.void else typ elts.(0) in
    let typ = Option.value typ'
        ~default:(Type.array ~elt:elt_type ~size:(Array.length elts)) in
    match typ with
    | Array { elt; size } ->
      assert (size = Array.length elts);
      let elts = Array.map elts ~f:(coerce ~typ:elt) in
      Array { loc; typ; elts }
    | _ -> assert false

  and subscript ~loc arg idx =
    let arg = to_value arg in
    typecheck_kind ~kind:Type.Kind.array arg;
    let idx = coerce idx ~typ:Type.int32 in
    Subscript { loc; arg; idx }

  and binop ~loc ~op ~lhs ~rhs =
    typecheck_kind ~kind:Type.Kind.numeric lhs;
    typecheck_kind ~kind:Type.Kind.numeric rhs;
    let typ = Type.union (typ lhs) (typ rhs) |> Option.value_exn in
    Binop { loc; op; lhs = coerce ~typ lhs; rhs = coerce ~typ rhs }

  and call ?loc callee args =
    let callee = to_value callee in
    typecheck_kind ~kind:Type.Kind.fun_ callee;
    let param_types = Type.params_exn @@ typ callee in
    match List.map2 param_types args ~f:(fun typ -> coerce ~typ) with
    | Ok args -> Call { loc; callee; args }
    | Unequal_lengths ->
      raise @@ Arity_mismatch {
        loc;
        expected = List.length param_types;
        got = List.length args
      }

    let let_in ?loc (binding: string * t) body =
      let ident, binding = binding in
      Let_in { loc; ident; binding; body }

  module Builder : sig
    type expr = t
    type t

    val build : t -> Env.t -> expr
    val of_ast : Ast.Expr.t -> t
  end = struct
    type expr = t
    type t = Env.t -> expr

    let build (builder: t) (env: Env.t) : expr = builder env

    let int ?loc value = fun _ -> int ?loc value

    let bool ?loc value = fun _ -> bool ?loc value

    let float ?loc value = fun _ -> float ?loc value

    let name ?loc ident = fun env ->
      match Env.lookup env ident with
      | Some { typ; pure } -> name ?loc ~typ ~pure ident
      | None -> raise @@ Unbound_identifier { loc; ident }

    let cast ?loc ~typ arg = fun env ->
      cast ?loc ~typ:(Type.Builder.build typ env) (build arg env)

    let array ?loc ~elts = fun env ->
      let elts = Array.map elts ~f:(fun elt -> build elt env) in
      array ?loc elts

    let subscript ~loc arg idx = fun env ->
      let arg = build arg env in
      let idx = build idx env in
      subscript ~loc arg idx

    let binop ~loc ~op ~lhs ~rhs = fun env ->
      binop ~loc ~op ~lhs:(lhs env) ~rhs:(rhs env)

    let call ?loc ~callee ~args = fun env ->
      call ?loc (callee env) (List.map args ~f:(fun arg -> arg env))

    let let_in ?binding_type ~loc:loc' ~ident ~binding ~body =
      let binding = match binding_type with
        | None ->
          if is_promotable binding
          then coerce ~typ:(Type.promote @@ typ binding) binding
          else binding
        | Some typ -> coerce ~typ binding in
      if not @@ is_pure binding
      then raise @@ Purity_error { loc = loc_exn binding };
      (let_in) ?loc:loc' (ident, binding) body

    let let_in ?binding_type ~loc ~ident ~binding ~body = fun env ->
      let binding_type = Option.map binding_type ~f:(fun typ -> Type.Builder.build typ env) in
      let binding = binding env in
      let env = Env.bind env ~ident ~typ:(typ binding) ~pure:true in
      let body = body env in
      (let_in) ?binding_type ~loc ~ident ~binding ~body

    let rec of_ast (expr: Ast.Expr.t) : t =
      match expr with
      | Int { loc; value } -> int ?loc value
      | Bool { loc; value } -> bool ?loc value
      | Float { loc; value } -> float ?loc value
      | Name { loc; ident } -> name ?loc ident
      | Array { loc; elts } -> array ?loc ~elts:(Array.map elts ~f:of_ast)
      | Subscript { loc; arg; idx } -> subscript ~loc (of_ast arg) (of_ast idx)
      | Cast { loc; arg; typ } -> cast ?loc (of_ast arg) ~typ:(Type.Builder.of_ast typ)
      | Binop { loc; op; lhs; rhs } ->
        let lhs = of_ast lhs in
        let rhs = of_ast rhs in
        binop ~loc ~op ~lhs ~rhs
      | Call { loc; callee; args } ->
        let callee = of_ast callee in
        let args = List.map args ~f:of_ast in
        call ?loc ~callee ~args
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
        loc: Srcloc.t option [@sexp.option];
        dst: Expr.t;
        src: Expr.t;
      }
    | Let of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Var of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | If of {
        loc: Srcloc.t option [@sexp.option];
        cond: Expr.t;
        iftrue: t;
        iffalse: t option [@sexp.option];
      }
    | While of {
        loc: Srcloc.t option [@sexp.option];
        cond: Expr.t;
        body: t;
      }
    | Return of {
        loc: Srcloc.t option [@sexp.option];
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
    | While { cond; body; _ } -> String.Set.union_list [Expr.impurities cond; impurities body]
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
      let dst = Expr.to_value dst in
      let src = Expr.coerce ~typ:(Expr.typ dst) src in
      assign ~loc ~dst ~src

    let assign ~loc ~dst ~src = fun env ->
      assign ~loc ~dst:(Expr.Builder.build dst env) ~src:(Expr.Builder.build src env), env

    let let_ ~loc ~typ ~ident ~binding =
      if Fn.non Expr.is_pure binding
      then raise @@ Purity_error { loc = Expr.loc_exn binding };
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

    let while_ ~loc ~cond ~body = fun env ->
      let cond = Expr.Builder.build cond env in
      let body, _ = body env in
      while_ ~loc ~cond ~body, env

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
      | While { loc; cond; body } ->
        let cond = Expr.Builder.of_ast cond in
        let body = of_ast body in
        while_ ~loc ~cond ~body
      | Return { loc; arg } ->
        let arg = Option.map arg ~f:Expr.Builder.of_ast in
        return ~loc ~arg
  end
end

module Decl = struct
  type t =
    | Type of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        binding: Type.t;
      }
    | Let of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Fun of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        params: string list;
        typ: Type.t;
        body: Stmt.t;
        pure: bool;
      }
    | Fun_expr of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        params: string list;
        typ: Type.t;
        body: Expr.t;
      }
    | Fun_extern of {
        loc: Srcloc.t option [@sexp.option];
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
      then raise @@ Purity_error { loc = Expr.loc_exn binding };
      (let_) ~loc ~ident ~typ ~binding:(Expr.coerce ~typ binding)

    let let_ ~loc ~ident ~typ ~binding = fun env ->
      let typ = Type.Builder.build typ env in
      let binding = Expr.Builder.build binding env in
      let env = Env.bind env ~ident ~typ ~pure:true in
      (let_) ~loc ~ident ~typ ~binding, env

    let fun_ ~loc ~ident ~params ~typ ~body ~pure =
      if pure && Fn.non Stmt.is_pure body
      then raise @@ Purity_error { loc = Option.value_exn loc };
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
      then raise @@ Purity_error { loc = Expr.loc_exn body };
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
