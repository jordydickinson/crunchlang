type t = private
  | Void
  | Bool
  | Int64
  | Float
  | Fun of {
      params: t list;
      ret: t;
    }
[@@deriving equal, sexp_of, variants]

val is_fun : t -> bool

val ret_exn : t -> t

val params : t -> t list option

val params_or_error : t -> t list Or_error.t

val params_exn : t -> t list
