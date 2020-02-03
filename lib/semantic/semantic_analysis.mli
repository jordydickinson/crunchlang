exception Unbound_identifier of {
    loc: Srcloc.t;
    ident: string;
  }

exception Type_error of {
    loc: Srcloc.t;
    expected: Type.t list;
    got: Type.t;
  }

exception Arity_mismatch of {
    loc: Srcloc.t;
    expected: int;
    got: int;
  }

(** [analyze_ast ast] constructs a [Semantic.t] by analyzing [ast].
    @raise [Unbound_identifier]
    @raise [Type_error]
    @raise [Arity_mismatch]
    @raise [Purity_error]  *)
val analyze_ast : Ast.t -> Semantic.t
