exception Unbound_identifier of {
    loc: Srcloc.t;
    ident: string;
  }

(** [analyze_ast ast] constructs a [Semantic.t] by analyzing [ast].
    @raise [Unbound_identifier]
    @raise [Type_error]
    @raise [Arity_mismatch]
    @raise [Purity_error]  *)
val analyze_ast : Ast.t -> Semantic.t
