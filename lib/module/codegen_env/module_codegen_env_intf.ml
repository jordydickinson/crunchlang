open Llvm

module type S = sig
  type t

  type binding =
    | Let of {
        value: llvalue;
        typ: Type.t;
      }
    | Var of {
        pointer: llvalue;
        typ: Type.t
      }

  val create : unit -> t

  val enter_scope : t -> unit
  val exit_scope : t -> unit

  val bind_let : t -> ident:string -> typ:Type.t -> value:llvalue -> unit
  val bind_var : t -> ident:string -> typ:Type.t -> pointer:llvalue -> unit

  val lookup : t -> string -> binding
end
