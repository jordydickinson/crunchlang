type t = private
  | Void
  | Bool
  | Int64
  | Float
  | Pointer of t
  | Array of t
  | Struct of (string * t) list
  | Fun of {
      params: t list;
      ret: t;
    }
[@@deriving equal, sexp_of]

val void : t
val bool : t
val int64 : t
val float : t
val pointer : t -> t
val array : t -> t
val struct_ : (string * t) list -> t
val fun_ : params:t list -> ret:t -> t

val is_fun : t -> bool
val ret_exn : t -> t
val params : t -> t list option
val params_or_error : t -> t list Or_error.t
val params_exn : t -> t list
val deref_exn : t -> t

module Kind : sig
  type concrete = t

  type t = private
    | Void
    | Bool
    | Numeric
    | Pointer
    | Array
    | Struct
    | Fun
  [@@deriving equal, sexp_of]

  val void : t
  val bool : t
  val numeric : t
  val pointer : t
  val array : t
  val struct_ : t
  val fun_ : t

  val of_type : concrete -> t
end

val is_kind : t -> Kind.t -> bool
