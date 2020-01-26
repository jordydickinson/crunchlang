open Llvm

let codegen (ast: Ast.t) ~module_ =
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
