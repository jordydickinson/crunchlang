type t =
  | Void
  | Bool
  | Int64
  | Float
  | Pointer of t
  | Array of t
  | Fun of {
      params: t list;
      ret: t;
    }
[@@deriving equal, sexp_of, variants]

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

let deref_exn typ =
  match typ with
  | Pointer typ -> typ
  | _ -> invalid_arg "Not a pointer type"

module Kind = struct
  type concrete = t
  [@@deriving equal, sexp_of]

  type t =
    | Void
    | Bool
    | Numeric
    | Pointer
    | Array
    | Fun
  [@@deriving equal, sexp_of, variants]

  let of_type (typ: concrete) =
    match typ with
    | Void -> Void
    | Bool -> Bool
    | Int64 | Float -> Numeric
    | Pointer _ -> Pointer
    | Array _ -> Array
    | Fun _ -> Fun
end

let is_kind typ kind =
  Kind.equal kind @@ Kind.of_type typ
