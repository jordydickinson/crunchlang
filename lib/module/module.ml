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

  let codegen_rvalue_name ~loc:_ ~ident ~builder =
    match Env.lookup env ident with
    | Let { value; typ } -> value, typ
    | Var { pointer; typ } -> build_load pointer ident builder, typ
  in

  let rec codegen_rvalue (expr: Ast.Expr.t) ~builder =
    let codegen_rvalue = codegen_rvalue ~builder in
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
    | Name { loc; ident } -> codegen_rvalue_name ~loc ~ident ~builder
    | Binop { loc = _; op = Add; lhs; rhs } ->
      let lhs, lhs_type = codegen_rvalue lhs in
      let rhs, rhs_type = codegen_rvalue rhs in
      if not @@ Type.equal lhs_type rhs_type then
        failwith "Type error"
      else if Type.equal lhs_type Type.Int64 then
        build_add lhs rhs "addtmp" builder, lhs_type
      else if Type.equal rhs_type Type.Float then
        build_fadd lhs rhs "faddtmp" builder, lhs_type
      else
        assert false
  in

  let codegen_lvalue_name ~loc:_ ~ident ~builder:_ =
    match Env.lookup env ident with
    | Let _ -> failwith "Not an lvalue"
    | Var { pointer; typ } -> pointer, typ
  in

  let codegen_lvalue (expr: Ast.Expr.t) ~builder =
    match expr with
    | Int _ | Float _ | Binop _ -> failwith "Not an lvalue"
    | Name { loc; ident } -> codegen_lvalue_name ~loc ~ident ~builder
  in

  let codegen_stmt (stmt: Ast.Stmt.t) ~builder =
    match stmt with
    | Let { loc = _; ident; typ = None; binding } ->
      let value, typ = codegen_rvalue binding ~builder in
      Env.bind_let env ~ident ~typ ~value
    | Let { loc = _; ident; typ = Some typ; binding } ->
      let typ = Type.of_type_expr typ in
      let value, value_type = codegen_rvalue binding ~builder in
      if not @@ Type.equal typ value_type
      then failwith "Type error"
      else Env.bind_let env ~ident ~typ ~value
    | Var { loc = _; ident; typ = None; binding } ->
      let value, typ = codegen_rvalue binding ~builder in
      let pointer = build_alloca (codegen_type typ) ident builder in
      ignore (build_store value pointer builder : llvalue);
      Env.bind_var env ~ident ~typ ~pointer
    | Var { loc = _; ident; typ = Some typ; binding } ->
      let typ = Type.of_type_expr typ in
      let value, value_type = codegen_rvalue binding ~builder in
      if not @@ Type.equal typ value_type then failwith "Type error";
      let pointer = build_alloca (codegen_type typ) ident builder in
      ignore (build_store value pointer builder : llvalue);
      Env.bind_var env ~ident ~typ ~pointer
    | Assign { loc = _; dst; src } ->
      let dst, dst_type = codegen_lvalue dst ~builder in
      let src, src_type = codegen_rvalue src ~builder in
      if not @@ Type.equal dst_type src_type
      then failwith "Type error"
      else ignore (build_store src dst builder : llvalue)
    | Return { loc = _; arg = None } ->
      if not @@ Type.equal !return_type Type.Void
      then failwith "Type error"
      else ignore (build_ret_void builder : llvalue)
    | Return { loc = _; arg = Some arg } ->
      let arg, typ = codegen_rvalue arg ~builder in
      if not @@ Type.equal !return_type typ
      then failwith "Type error"
      else ignore (build_ret arg builder : llvalue)
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
    | Fun { loc = _; name; params; ret_type; body } ->
      let ret_type = Type.of_type_expr ret_type in
      return_type := ret_type;
      let param_types = Array.of_list_map params ~f:(Fn.compose codegen_type_expr snd) in
      let typ = function_type (codegen_type ret_type) param_types in
      let func = define_function name typ module_ in
      let entry = entry_block func in
      let builder = builder_at_end (module_context module_) entry in
      Env.enter_scope env;
      List.iteri params ~f:begin fun i (ident, typ) ->
        let value = param func i in
        set_value_name ident value;
        Env.bind_let env ~ident ~typ:(Type.of_type_expr typ) ~value
      end;
      codegen_block body ~builder;
      Env.exit_scope env;
  in

  List.iter ast ~f:codegen_decl
