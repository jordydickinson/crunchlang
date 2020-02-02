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

exception Purity_error of {
    loc: Srcloc.t
  }

(** [infer ast] constructs a [Purity.t] tree by inferring the purity of
    subexpressions.
    @raise [Unbound_identifier]
    @raise [Type_error]
    @raise [Arity_mismatch]
    @raise [Purity_error]  *)
val infer : Ast.t -> Purity.t
