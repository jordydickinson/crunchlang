type t = private
  | Void
  | Bool
  | Int of { bitwidth: int; signed: bool }
  | Float32
  | Float64
  | Reference of t
  | Array of { elt: t; size: int }
  | Struct of (string * t) list
  | Fun of {
      params: t list;
      ret: t;
    }
[@@deriving equal, compare, hash, sexp_of]

val void : t
val bool : t
val uint8 : t
val int32 : t
val int64 : t
val float32 : t
val float64 : t
val reference : t -> t
val array : elt:t -> size:int -> t
val struct_ : (string * t) list -> t
val fun_ : params:t list -> ret:t -> t

val union : t -> t -> t option

val is_fun : t -> bool
val is_reference : t -> bool
val is_numeric : t -> bool

val ret_exn : t -> t
val params : t -> t list option
val params_or_error : t -> t list Or_error.t
val params_exn : t -> t list
val elt_exn : t -> t
val deref_exn : t -> t
val is_signed_exn : t -> bool

module Kind : sig
  type concrete = t

  type t = private
    | Void
    | Bool
    | Numeric
    | Reference
    | Array
    | Struct
    | Fun
  [@@deriving equal, sexp_of]

  val void : t
  val bool : t
  val numeric : t
  val reference : t
  val array : t
  val struct_ : t
  val fun_ : t

  val of_type : concrete -> t
end

val is_kind : t -> Kind.t -> bool
