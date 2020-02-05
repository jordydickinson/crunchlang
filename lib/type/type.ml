type t =
  | Void
  | Bool
  | Int64
  | Float
  | Fun of {
      params: t list;
      ret: t;
    }
[@@deriving equal, sexp_of, variants]

let of_type_expr (type_expr: Ast.Type_expr.t) =
  match type_expr with
  | Void _ -> Void
  | Bool _ -> Bool
  | Int64 _ -> Int64
  | Float _ -> Float
  | Name _ -> assert false

let is_fun = function
  | Fun _ -> true
  | _ -> false

let ret_exn = function
  | Fun { ret; _ } -> ret
  | _ -> invalid_arg "Not a function type"

let params = function
  | Fun { params; _ } -> Some params
  | _ -> None

let params_or_error typ =
  match params typ with
  | Some params -> Ok params
  | None -> error "Not a function type" typ sexp_of_t

let params_exn typ =
  ok_exn @@ params_or_error typ
