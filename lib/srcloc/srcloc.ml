module T = struct
  type t = Source_code_position.t * Source_code_position.t
  [@@deriving equal, compare, hash, sexp_of]
end

include T
include Comparable.Make_plain(T)
include Hashable.Make_plain(T)
