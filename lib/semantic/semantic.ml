module Bop = struct
  type t =
    | Add
    | Fadd
  [@@deriving sexp_of, variants]
end

module Pure_expr = struct
  type t =
    | Int of {
        loc: Srcloc.t;
        value: int64
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
      }
    | Binop of {
        loc: Srcloc.t;
        op: Bop.t;
        lhs: t;
        rhs: t;
        typ: Type.t
      }
    | Call of {
        loc: Srcloc.t;
        callee: t;
        args: t list;
        typ: Type.t;
      }
  [@@deriving sexp_of, variants]

  let loc = function
    | Int { loc; _ }
    | Bool { loc; _ }
    | Float { loc; _ }
    | Name { loc; _ }
    | Binop { loc; _ }
    | Call { loc; _ } -> loc

  let typ = function
    | Int _ -> Type.int64
    | Bool _ -> Type.bool
    | Float _ -> Type.float
    | Name { typ; _ }
    | Binop { typ; _ }
    | Call { typ; _ } -> typ
end

module Expr = struct
  type t =
    | Pure of Pure_expr.t
    | Name of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
      }
    | Binop of {
        loc: Srcloc.t;
        op: Bop.t;
        lhs: t;
        rhs: t;
        typ: Type.t;
      }
    | Call of {
        loc: Srcloc.t;
        callee: t;
        args: t list;
        typ: Type.t;
      }
  [@@deriving sexp_of, variants]

  let loc = function
    | Pure pure -> Pure_expr.loc pure
    | Name { loc; _ }
    | Binop { loc; _ }
    | Call { loc; _ } -> loc

  let typ = function
    | Pure pure -> Pure_expr.typ pure
    | Name { typ; _ }
    | Binop { typ; _ }
    | Call { typ; _ } -> typ

  let is_pure = function
    | Pure _ -> true
    | _ -> false

  let pure_expr_exn = function
    | Pure expr -> expr
    | _ -> invalid_arg "Non-const"

  let rec lift_pure_exprs expr =
    match expr with
    | Pure _
    | Name _ -> expr
    | Binop { loc; op; lhs; rhs; typ } ->
      let lhs = lift_pure_exprs lhs in
      let rhs = lift_pure_exprs rhs in
      if is_pure lhs && is_pure rhs then
        pure
        @@ Pure_expr.binop ~loc ~op
          ~lhs:(pure_expr_exn lhs)
          ~rhs:(pure_expr_exn rhs)
          ~typ
      else
        binop ~loc ~op ~lhs ~rhs ~typ
    | Call { loc; callee; args; typ } ->
      let callee = lift_pure_exprs callee in
      let args = List.map args ~f:lift_pure_exprs in
      if is_pure callee && List.for_all args ~f:is_pure then
        let callee = pure_expr_exn callee in
        let args = List.map args ~f:pure_expr_exn in
        pure @@ Pure_expr.call ~loc ~callee ~args ~typ
      else
        call ~loc ~callee ~args ~typ
end

module Stmt = struct
  type t =
    | Expr of Expr.t
    | Block of t list
    | Let of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: Pure_expr.t;
      }
    | Var of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Assign of {
        loc: Srcloc.t;
        dst: Expr.t;
        src: Expr.t;
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
end

module Decl = struct
  type t =
    | Fun of {
        loc: Srcloc.t;
        ident: string;
        params: string list;
        typ: Type.t;
        body: Stmt.t;
      }
  [@@deriving sexp_of, variants]

  let typ = function
    | Fun { typ; _ } -> typ
end

type t = Decl.t list
