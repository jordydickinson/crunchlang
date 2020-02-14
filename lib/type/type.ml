type t =
  | Void
  | Bool
  | Int of { bitwidth: int; signed: bool }
  | Float
  | Pointer of t
  | Array of { elt: t; size: int }
  | Struct of (string * t) list
  | Fun of {
      params: t list;
      ret: t;
    }
[@@deriving equal, compare, hash, sexp_of, variants]

let uint8 = int ~bitwidth:8 ~signed:false
let int32 = int ~bitwidth:32 ~signed:true
let int64 = int ~bitwidth:64 ~signed:true

let rec union typ typ' =
  match typ, typ' with
  | _ when equal typ typ' -> Some typ
  | Void, typ
  | typ, Void -> Some typ
  | Int { bitwidth; signed }, Int { bitwidth = bitwidth'; signed = signed' } ->
    Option.some @@ int ~bitwidth:(max bitwidth bitwidth') ~signed:(signed || signed')
  | Array { size = size; elt }, Array { size = size'; elt = elt' } ->
    let%map.Option elt = union elt elt' in array ~size:(max size size') ~elt
  | Bool, _
  | Float, _
  | Int _, _
  | Array _, _
  | Pointer _, _
  | Struct _, _
  | Fun _, _  -> None

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

let is_signed_exn = function
  | Int { signed; _ } -> signed
  | _ -> invalid_arg "Not an integer type"

module Kind = struct
  type concrete = t
  [@@deriving equal, sexp_of]

  type t =
    | Void
    | Bool
    | Numeric
    | Pointer
    | Array
    | Struct
    | Fun
  [@@deriving equal, sexp_of, variants]

  let of_type (typ: concrete) =
    match typ with
    | Void -> Void
    | Bool -> Bool
    | Int _ | Float -> Numeric
    | Pointer _ -> Pointer
    | Array _ -> Array
    | Struct _ -> Struct
    | Fun _ -> Fun
end

let is_kind typ kind =
  Kind.equal kind @@ Kind.of_type typ
