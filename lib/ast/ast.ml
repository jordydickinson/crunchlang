module Expr = struct
  type t =
    | Int of {
        loc: Srcloc.t [@equal.ignore];
        value: int64
      }
    | Float of {
        loc: Srcloc.t [@equal.ignore];
        value: float;
      }
    | Name of {
        loc: Srcloc.t [@equal.ignore];
        ident: string;
      }
  [@@deriving equal, sexp_of, variants]
end
