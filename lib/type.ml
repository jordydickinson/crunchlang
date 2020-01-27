type t =
  | Void
  | Int64
  | Float
  | Fun of {
      params: t list;
      ret: t;
    }
[@@deriving equal, variants]

let of_type_expr (type_expr: Ast.Type_expr.t) =
  match type_expr with
  | Void _ -> Void
  | Int64 _ -> Int64
  | Float _ -> Float

let is_fun = function
  | Fun _ -> true
  | _ -> false

let ret_exn = function
  | Fun { ret; _ } -> ret
  | _ -> invalid_arg "Not a function type"

let params_exn = function
  | Fun { params; _ } -> params
  | _ -> invalid_arg "Not a function type"
