module Kind = struct
  type t =
    | Void
    | Bool
    | Numeric
    | Pointer
    | Array
    | Struct
    | Fun
  [@@deriving equal, sexp_of, variants]
end

module Concrete = struct
  type t =
    | Void
    | Bool
    | Int64
    | Float
    | Pointer of t
    | Array of t
    | Struct of (string * t) list
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

  let kind = function
    | Void -> Kind.void
    | Bool -> Kind.bool
    | Int64 | Float -> Kind.numeric
    | Pointer _ -> Kind.pointer
    | Array _ -> Kind.array
    | Struct _ -> Kind.struct_
    | Fun _ -> Kind.fun_

  let is_kind typ kind' =
    Kind.equal kind' @@ kind typ
end

type t = Concrete.t
[@@deriving equal, sexp_of]

let void = Concrete.void
let bool = Concrete.bool
let int64 = Concrete.int64
let float = Concrete.float
let pointer = Concrete.pointer
let array = Concrete.array
let struct_ = Concrete.struct_
let fun_ = Concrete.fun_
let kind = Concrete.kind
let is_kind = Concrete.is_kind
let is_fun = Concrete.is_fun
let ret_exn = Concrete.ret_exn
let params = Concrete.params
let params_or_error = Concrete.params_or_error
let params_exn = Concrete.params_exn
let deref_exn = Concrete.deref_exn

module Parameterized = struct
  type t =
    | Void
    | Bool
    | Int64
    | Float
    | Arg of int
    | Pointer of t
    | Array of t
    | Struct of (string * t) list
    | Fun of { params: t list; ret: t }
  [@@deriving sexp, variants]

  let rec of_concrete (typ: Concrete.t) =
    match typ with
    | Void -> Void
    | Bool -> Bool
    | Int64 -> Int64
    | Float -> Float
    | Pointer typ -> Pointer (of_concrete typ)
    | Array typ -> Array (of_concrete typ)
    | Struct params -> Struct (List.map params ~f:(Tuple2.map_snd ~f:of_concrete))
    | Fun { params; ret } -> Fun { params = List.map params ~f:of_concrete
                                 ; ret = of_concrete ret }

  let rec instantiate_exn typ ~args =
    match typ with
    | Void -> Concrete.void
    | Bool -> Concrete.bool
    | Int64 -> Concrete.int64
    | Float -> Concrete.float
    | Arg i -> args.(i)
    | Pointer typ ->
      Concrete.pointer @@ instantiate_exn typ ~args
    | Array typ -> Concrete.array @@ instantiate_exn typ ~args
    | Struct fields -> Concrete.struct_ @@ List.map fields
        ~f:(Tuple2.map_snd ~f:(instantiate_exn ~args))
    | Fun { params; ret } ->
      Concrete.fun_ ~params:(List.map params ~f:(instantiate_exn ~args))
        ~ret:(instantiate_exn ret ~args)
end
