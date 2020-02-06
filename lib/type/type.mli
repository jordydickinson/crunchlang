type t = private
  | Void
  | Bool
  | Int64
  | Float
  | Pointer of t
  | Array of t
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

val deref_exn : t -> t

module Kind : sig
  type concrete = t
  [@@deriving equal, sexp_of]

  type t = private
    | Void
    | Bool
    | Numeric
    | Pointer
    | Array
    | Fun
  [@@deriving equal, sexp_of, variants]

  val of_type : concrete -> t
end

val is_kind : t -> Kind.t -> bool
