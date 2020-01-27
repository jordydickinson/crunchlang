module Type_expr = struct
  type t =
    | Void of { loc: Srcloc.t }
    | Int64 of { loc: Srcloc.t }
    | Float of { loc: Srcloc.t }
  [@@deriving sexp_of, variants]
end

module Expr = struct
  module Bop = struct
    type t =
      | Add
    [@@deriving sexp_of, variants]
  end

  type t =
    | Int of {
        loc: Srcloc.t;
        value: int64
      }
    | Float of {
        loc: Srcloc.t;
        value: float;
      }
    | Name of {
        loc: Srcloc.t;
        ident: string;
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
  [@@deriving sexp_of, variants]

  let add = binop ~op:Bop.add
end

module Stmt = struct
  type t =
    | Expr of Expr.t
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
    | Assign of {
        loc: Srcloc.t;
        dst: Expr.t;
        src: Expr.t;
      }
    | Return of {
        loc: Srcloc.t;
        arg: Expr.t option [@sexp.option];
      }
  [@@deriving sexp_of, variants]
end

module Decl = struct
  type t =
    | Fun of {
        loc: Srcloc.t;
        name: string;
        params: (string * Type_expr.t) list;
        ret_type: Type_expr.t;
        body: Stmt.t list;
      }
  [@@deriving sexp_of, variants]
end

type t = Decl.t list
