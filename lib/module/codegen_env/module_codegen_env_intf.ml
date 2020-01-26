open Llvm

module type S = sig
  type t

  val create : unit -> t

  val enter_scope : t -> unit
  val exit_scope : t -> unit

  val bind : t -> ident:string -> typ:Type.t -> value:llvalue -> unit

  val lookup : t -> string -> llvalue * Type.t
end
