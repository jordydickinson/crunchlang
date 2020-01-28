module type S = sig
  (** [infer ast] constructs a [Purity.t] by inferring the purity of
      expressions. It is an error if [ast] is ill-typed or contains references
      to unbound identifiers. *)
  val infer : Ast.t -> Purity.t Or_error.t
end
