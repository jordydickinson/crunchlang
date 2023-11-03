include Llvm

let is_integer_type ?bitwidth typ =
  match classify_type typ, bitwidth with
  | TypeKind.Integer, None -> true
  | TypeKind.Integer, Some bitwidth ->
    integer_bitwidth typ = bitwidth
  | _ -> false

let is_integer ?bitwidth value =
  is_integer_type ?bitwidth @@ type_of value

let is_fp_type typ =
  match classify_type typ with
  | TypeKind.Half
  | TypeKind.Float
  | TypeKind.Double
  | TypeKind.Fp128
  | TypeKind.X86fp80
  | TypeKind.Ppc_fp128 -> true
  | _ -> false

let is_fp value =
  is_fp_type @@ type_of value

let is_function_type typ =
  match classify_type typ with
  | TypeKind.Function -> true
  | _ -> false

let is_pointer_type typ =
  match classify_type typ with
  | TypeKind.Pointer -> true
  | _ -> false

let is_pointer value =
  is_pointer_type @@ type_of value

let is_struct_type typ =
  match classify_type typ with
  | TypeKind.Struct -> true
  | _ -> false

let is_struct value = is_struct_type @@ type_of value

let equal_lltype typ typ' =
  Poly.equal (classify_type typ) (classify_type typ')

let const_int typ value =
  assert (is_integer_type typ);
  const_int typ value

let const_of_int64 typ value =
  assert (is_integer_type typ);
  const_of_int64 typ value

let const_float typ value =
  assert (is_fp_type typ);
  const_float typ value

let build_load typ src name builder =
  assert (is_pointer src);
  build_load typ src name builder

let build_store src dst builder =
  assert (is_pointer dst);
  build_store src dst builder

let build_cond_br cond iftrue iffalse builder =
  assert (is_integer cond ~bitwidth:1);
  build_cond_br cond iftrue iffalse builder

let build_add lhs rhs name builder =
  assert (is_integer lhs);
  assert (is_integer rhs);
  build_add lhs rhs name builder

let build_fadd lhs rhs name builder =
  assert (is_fp lhs);
  assert (is_fp rhs);
  build_fadd lhs rhs name builder

let build_struct_gep value idx name builder =
  assert (is_pointer value);
  build_struct_gep (type_of value) value idx name builder

let build_bitcast value typ name builder =
  assert (Poly.equal (size_of typ) (size_of @@ type_of value));
  build_bitcast value typ name builder

let append_block ctx name func =
  assert (is_pointer func);
  append_block ctx name func

let define_function name typ m =
  assert (is_function_type typ);
  define_function name typ m
