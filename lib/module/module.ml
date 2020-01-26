open Llvm

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
  let codegen_type_expr (type_expr: Ast.Type_expr.t) =
    match type_expr with
    | Void { loc = _ } -> void_type (module_context module_)
    | Int64 { loc = _ } -> i64_type (module_context module_)
  in

  let rec codegen_expr (expr: Ast.Expr.t) ~builder =
    let codegen_expr = codegen_expr ~builder in
    match expr with
    | Int { loc = _; value } ->
      const_of_int64
        (i64_type @@ module_context module_)
        value
        true (* Signed *)
    | Binop { loc = _; op = Add; lhs; rhs } ->
      let lhs = codegen_expr lhs in
      let rhs = codegen_expr rhs in
      build_add lhs rhs "addtmp" builder
    | _ -> assert false
  in

  let codegen_stmt (stmt: Ast.Stmt.t) ~builder =
    match stmt with
    | Return { loc = _; arg = None } ->
      build_ret_void builder
    | Return { loc = _; arg = Some arg } ->
      let arg = codegen_expr arg ~builder in
      build_ret arg builder
    | _ -> assert false
  in

  let rec codegen_block (block: Ast.Stmt.t list) ~builder =
    match block with
    | [] -> ()
    | stmt :: block ->
      let _ : llvalue = codegen_stmt stmt ~builder in
      codegen_block block ~builder
  in

  let codegen_decl (decl: Ast.Decl.t) =
    match decl with
    | Fun { loc = _; name; params = []; ret_type; body } ->
      let typ = function_type (codegen_type_expr ret_type) [||] in
      let func = define_function name typ module_ in
      let entry = entry_block func in
      let builder = builder_at_end (module_context module_) entry in
      codegen_block body ~builder
    | _ -> assert false
  in

  List.iter ast ~f:codegen_decl
