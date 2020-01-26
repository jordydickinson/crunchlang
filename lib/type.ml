type t =
  | Void
  | Int64
  | Float
[@@deriving equal]

let of_type_expr (type_expr: Ast.Type_expr.t) =
  match type_expr with
  | Void _ -> Void
  | Int64 _ -> Int64
  | Float _ -> Float
