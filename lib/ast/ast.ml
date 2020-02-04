module Type_expr = struct
  type t =
    | Void of { loc: Srcloc.t }
    | Bool of { loc: Srcloc.t }
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
    | Var_in of {
        loc: Srcloc.t;
        ident: string;
        typ: Type_expr.t option [@sexp.option];
        binding: t;
        body: t;
      }
  [@@deriving sexp_of, variants]

  let add = binop ~op:Bop.add

  let loc = function
    | Int { loc; _ }
    | Bool { loc; _ }
    | Float { loc; _ }
    | Name { loc; _ }
    | Binop { loc; _ }
    | Call { loc; _ }
    | Let_in { loc; _ }
    | Var_in { loc; _ } -> loc
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

  let to_block stmt =
    match stmt with
    | Block _ -> stmt
    | _ -> Block [stmt]
end

module Decl = struct
  type t =
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
      }
  [@@deriving sexp_of, variants]

  let loc = function
    | Let { loc; _ }
    | Fun { loc; _ } -> loc
end

type t = Decl.t list
