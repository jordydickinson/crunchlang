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
  [@@deriving sexp_of, variants]

  let add = binop ~op:Bop.add
end
