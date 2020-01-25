module type S = sig
  type t = Source_code_position.t * Source_code_position.t
  [@@deriving sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end
