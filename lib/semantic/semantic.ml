module Bop = struct
  type t =
    | Add
    | Fadd
  [@@deriving sexp_of, variants]
end

module Expr = struct
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
        typ: Type.t;
        pure: bool;
      }
    | Call of {
        loc: Srcloc.t;
        callee: t;
        args: t list;
        typ: Type.t;
        pure: bool;
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

  let is_pure = function
    | Int _
    | Bool _
    | Float _ -> true
    | Name { pure; _ }
    | Binop { pure; _ }
    | Call { pure; _ } -> pure
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
end

type t = Decl.t list
