module Bop : sig
  type t =
    | Add
    | Fadd
end

module Expr : sig
  type t = private
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
    | Let_in of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: t;
        body: t;
        pure: bool;
      }
  [@@deriving sexp_of, variants]

  val loc : t -> Srcloc.t

  val typ : t -> Type.t

  val is_pure : t -> bool
end

module Stmt : sig
  type t = private
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

  val to_block : t -> t
end

module Decl : sig
  type t = private
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

  val typ : t -> Type.t
end

type t = Decl.t list
