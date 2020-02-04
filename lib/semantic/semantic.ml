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
    | Assign of {
        loc: Srcloc.t;
        dst: t;
        src: t;
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
    | Var_in of {
        loc: Srcloc.t;
        ident: string;
        binding: t;
        body: t;
      }
  [@@deriving sexp_of, variants]

  let loc = function
    | Int { loc; _ }
    | Bool { loc; _ }
    | Float { loc; _ }
    | Name { loc; _ }
    | Binop { loc; _ }
    | Assign { loc; _ }
    | Call { loc; _ }
    | Let_in { loc; _ }
    | Var_in { loc; _ } -> loc

  let rec typ = function
    | Int _ -> Type.int64
    | Bool _ -> Type.bool
    | Float _ -> Type.float
    | Assign _ -> Type.void
    | Name { typ; _ } -> typ
    | Binop { lhs; _ } -> typ lhs
    | Call { callee; _ } -> Type.ret_exn @@ typ callee
    | Let_in { body; _ }
    | Var_in { body; _ } -> typ body

  let rec impurities = function
    | Int _ | Bool _ | Float _ -> String.Set.empty
    | Name { ident; pure; _ } ->
      if pure then String.Set.empty else String.Set.singleton ident
    | Binop { lhs; rhs; _ } -> Set.union (impurities lhs) (impurities rhs)
    | Assign { dst; src; _ } -> Set.union (impurities dst) (impurities src)
    | Call { callee; args; _ } ->
      String.Set.union_list (impurities callee :: List.map ~f:impurities args)
    | Let_in { body; _ } -> impurities body
    | Var_in { ident; binding; body; _ } ->
      Set.remove (Set.union (impurities binding) (impurities body)) ident

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

  let binop ~loc ~op:Bop.Add ~lhs ~rhs =
    typecheck_or lhs ~types:[Type.int64; Type.float];
    typecheck rhs ~typ:(typ lhs);
    binop ~loc ~op:Bop.Add ~lhs ~rhs

  let assign ~loc ~src ~dst =
    typecheck dst ~typ:(typ src);
    assign ~loc ~src ~dst

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

  let let_in ?binding_type ~loc:loc' ~ident ~binding ~body =
    Option.iter binding_type ~f:(fun typ -> typecheck ~typ binding);
    if not @@ is_pure binding
    then raise @@ Purity_error { loc = loc binding };
    (let_in) ~loc:loc' ~ident ~binding ~body

  let var_in ?binding_type ~loc ~ident ~binding ~body =
    Option.iter binding_type ~f:(fun typ -> typecheck ~typ binding);
    var_in ~loc ~ident ~binding ~body
end

module Stmt = struct
  type t =
    | Expr of Expr.t
    | Block of t list
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

  let to_block = function
    | Block _ as stmt -> stmt
    | stmt -> Block [stmt]

  let let_ ~loc ~typ ident binding =
    if Fn.non Expr.is_pure binding
    then raise @@ Purity_error { loc = Expr.loc binding };
    Expr.typecheck ~typ binding;
    (let_) ~loc ~ident ~typ ~binding

  let var ~loc ~typ ident binding =
    Expr.typecheck ~typ binding;
    var ~loc ~ident ~typ ~binding
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
      }
  [@@deriving sexp_of, variants]

  let typ = function
    | Let { typ; _ } -> typ
    | Fun { typ; _ } -> typ

  let let_ ~loc ~ident ~typ ~binding =
    if Fn.non Expr.is_pure binding
    then raise @@ Purity_error { loc = Expr.loc binding };
    Expr.typecheck ~typ binding;
    (let_) ~loc ~ident ~typ ~binding
end

type t = Decl.t list
