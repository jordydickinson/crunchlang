type binding = {
  typ: Type.t;
  pure: bool;
}

(** An environment used during purity inference. *)
type t

(** [create ()] is a new environment. *)
val create : unit -> t

(** [scoped env ~f] is [f] evaluated in a new scope. Anything bound by [f] is
    discarded when it returns. *)
val scoped : t -> f:(unit -> 'a) -> 'a

(** [bind env ~ident ~typ ~pure] binds [ident] in the most-recently-opened
    scope with type [typ] and purity [pure].
    @param [pure] If the binding is pure, then [true], else [false]. *)
val bind : t -> ident:string -> typ:Type.t -> pure:bool -> unit

(** [lookup env ident] is [Some binding] if [ident] is bound in [env] and
    [None] otherwise. *)
val lookup : t -> string -> binding option
