module Type_expr : sig
  type t = private
    | Name of { loc: Srcloc.t; ident: string }
    | Apply of { loc: Srcloc.t; ident: string; args: t array }
  [@@deriving sexp_of, variants]
end

module Expr : sig
  module Bop : sig
    type t =
      | Add
    [@@deriving sexp_of, variants]
  end

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
      }
    | Array of {
        loc: Srcloc.t;
        elts: t array;
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
        typ: Type_expr.t option [@sexp.option];
        binding: t;
        body: t;
      }
  [@@deriving sexp_of, variants]

  val add : loc:Srcloc.t -> lhs:t -> rhs:t -> t

  val loc : t -> Srcloc.t
end

module Stmt : sig
  type t = private
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
        typ: Type_expr.t option [@sexp.option];
        binding: Expr.t;
      }
    | Var of {
        loc: Srcloc.t;
        ident: string;
        typ: Type_expr.t option [@sexp.option];
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

  val to_block : t -> t
end

module Decl : sig
  type t = private
    | Type of {
        loc: Srcloc.t;
        ident: string;
        binding: Type_expr.t;
      }
    | Let of {
        loc: Srcloc.t;
        ident: string;
        typ: Type_expr.t;
        binding: Expr.t;
      }
    | Fun of {
        loc: Srcloc.t;
        ident: string;
        params: (string * Type_expr.t) list;
        ret_type: Type_expr.t;
        body: Stmt.t;
        pure: bool [@sexp.bool];
      }
    | Fun_expr of {
        loc: Srcloc.t;
        ident: string;
        params: (string * Type_expr.t) list;
        ret_type: Type_expr.t;
        body: Expr.t;
      }
  [@@deriving sexp_of, variants]

  val loc : t -> Srcloc.t
end

type t = Decl.t list
