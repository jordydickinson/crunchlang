open Llvm

module type S = sig
  (** The code generation environment. It keeps track of bound names, their
      types, and their values. *)
  type t

  (** A name binding *)
  type binding =
    | Let of {
        value: llvalue; (** The (constant) value of this declaration *)
        typ: Type.t;    (** The binding's type *)
      } (** A let-declaration binding *)
    | Var of {
        pointer: llvalue; (** The location in memory where the value is stored. *)
        typ: Type.t       (** The binding's type *)
      } (** A var-declaration binding *)

  (** [create ()] creates a new environment in global scope *)
  val create : unit -> t

  (** [scoped env ~f] evaluates [f] in a new scope, discarding its scoped
      bindings when [f] returns. *)
  val scoped : t -> f:(unit -> 'a) -> 'a

  (** [bind_let env ~ident ~typ ~value] enters the binding
      [let ident: typ = value] into the most-recently-opened scope. *)
  val bind_let : t -> ident:string -> typ:Type.t -> value:llvalue -> unit

  (** [bind_var env ~ident ~typ ~pointer] enters the binding [var ident: typ]
      into the most-recently-opened scope.
      @param pointer A pointer to the memory location where [ident] is
        allocated. *)
  val bind_var : t -> ident:string -> typ:Type.t -> pointer:llvalue -> unit

  (** [lookup env ident] is the binding associated with [ident]. An exception
      is raised if [ident] is not in scope. *)
  val lookup : t -> string -> binding
end
