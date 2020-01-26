open Llvm

module Env = Module_codegen_env

let () = enable_pretty_stacktrace ()

type t = llmodule

let with_new_module name ~f =
  let context = create_context () in
  let module_ = create_module context name in
  let ret = f module_ in
  dispose_module module_;
  dispose_context context;
  ret

let llir_string = string_of_llmodule

let write_llir module_ filename =
  print_module filename module_

let dump_llir = dump_module

let init = lazy (Llvm_all_backends.initialize ())

let get_triple () =
  Lazy.force init;
  Llvm_target.Target.default_triple ()

let get_target ~triple =
  let target = Llvm_target.Target.by_triple triple in
  Llvm_target.TargetMachine.create ~triple target

let emit_obj module_ ~filename =
  let triple = get_triple () in
  let target = get_target ~triple in
  Llvm_target.TargetMachine.emit_to_file
    module_
    Llvm_target.CodeGenFileType.ObjectFile
    filename
    target

let codegen module_ (ast: Ast.t) =
  let env = Env.create () in
  let return_type = ref Type.Void in

  let codegen_type (typ: Type.t) =
    match typ with
    | Void -> void_type (module_context module_)
    | Int64 -> i64_type (module_context module_)
    | Float -> double_type (module_context module_)
  in

  let codegen_type_expr (type_expr: Ast.Type_expr.t) =
    codegen_type @@ Type.of_type_expr type_expr
  in

  let rec codegen_expr (expr: Ast.Expr.t) ~builder =
    let codegen_expr = codegen_expr ~builder in
    match expr with
    | Int { loc = _; value } ->
      let typ = Type.Int64 in
      let value =
        const_of_int64
          (codegen_type typ)
          value
          true (* Signed *)
      in
      value, typ
    | Float { loc = _; value } ->
      let typ = Type.Float in
      let value =
        const_float
          (codegen_type typ)
          value
      in
      value, typ
    | Name { loc = _; ident } ->
      Env.lookup env ident
    | Binop { loc = _; op = Add; lhs; rhs } ->
      let lhs, lhs_type = codegen_expr lhs in
      let rhs, rhs_type = codegen_expr rhs in
      if not @@ Type.equal lhs_type rhs_type
      then failwith "Type error"
      else build_add lhs rhs "addtmp" builder, lhs_type
  in

  let codegen_stmt (stmt: Ast.Stmt.t) ~builder =
    match stmt with
    | Let { loc = _; ident; typ = None; binding } ->
      let value, typ = codegen_expr binding ~builder in
      Env.bind env ~ident ~typ ~value
    | Let { loc = _; ident; typ = Some typ; binding } ->
      let typ = Type.of_type_expr typ in
      let value, value_type = codegen_expr binding ~builder in
      if not @@ Type.equal typ value_type
      then failwith "Type error"
      else Env.bind env ~ident ~typ ~value
    | Return { loc = _; arg = None } ->
      if not @@ Type.equal !return_type Type.Void
      then failwith "Type error"
      else ignore (build_ret_void builder : llvalue)
    | Return { loc = _; arg = Some arg } ->
      let arg, typ = codegen_expr arg ~builder in
      if not @@ Type.equal !return_type typ
      then failwith "Type error"
      else ignore (build_ret arg builder : llvalue)
    | _ -> assert false
  in

  let rec codegen_block (block: Ast.Stmt.t list) ~builder =
    match block with
    | [] -> ()
    | stmt :: block ->
      codegen_stmt stmt ~builder;
      codegen_block block ~builder
  in

  let codegen_decl (decl: Ast.Decl.t) =
    match decl with
    | Fun { loc = _; name; params = []; ret_type; body } ->
      return_type := Type.of_type_expr ret_type;
      let typ = function_type (codegen_type_expr ret_type) [||] in
      let func = define_function name typ module_ in
      let entry = entry_block func in
      let builder = builder_at_end (module_context module_) entry in
      codegen_block body ~builder
    | _ -> assert false
  in

  List.iter ast ~f:codegen_decl
