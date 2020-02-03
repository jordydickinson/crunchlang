open LLVM

module Env = Codegen_env

let init = lazy (Llvm_all_backends.initialize ())

let global_context = LLVM.global_context

let create_context = LLVM.create_context

let create_module = LLVM.create_module

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

type binding =
  | Value of llvalue
  | Pointer of llvalue

let codegen_cf module_ (cf: Control_flow.t) =
  let module Bop = Control_flow.Bop in
  let module Expr = Control_flow.Expr in

  let names = String.Table.create () in

  let rec codegen_type (typ: Type.t) =
    match typ with
    | Void -> void_type (module_context module_)
    | Bool -> i1_type (module_context module_)
    | Int64 -> i64_type (module_context module_)
    | Float -> double_type (module_context module_)
    | Fun { params; ret } ->
      function_type (codegen_type ret)
      @@ Array.of_list_map params ~f:codegen_type
  in

  let codegen_rvalue_name ident ~builder =
    match Hashtbl.find_exn names ident with
    | Value v -> v
    | Pointer p -> build_load p ident builder
  in

  let codegen_bop (op: Bop.t) lhs rhs ~builder =
    match op with
    | Add -> build_add lhs rhs "addtmp" builder
    | Fadd -> build_fadd lhs rhs "addtmp" builder
  in

  let rec codegen_rvalue (expr: Expr.t) ~builder =
    let codegen_rvalue = codegen_rvalue ~builder in
    match expr with
    | Int { value; _ } ->
      const_of_int64 (codegen_type @@ Expr.typ expr) value
        true (* Signed *)
    | Bool { value; _ } ->
      const_int (codegen_type @@ Expr.typ expr)
        (if value then 1 else 0)
    | Float { value; _ } ->
      const_float (codegen_type @@ Expr.typ expr) value
    | Name { ident; _ } -> codegen_rvalue_name ident ~builder
    | Binop { op; lhs; rhs; _ } ->
      let lhs = codegen_rvalue lhs in
      let rhs = codegen_rvalue rhs in
      codegen_bop op lhs rhs ~builder
    | Call { callee; args; _ } ->
      let callee = codegen_rvalue callee in
      let args = Array.of_list_map args ~f:codegen_rvalue in
      build_call callee args "calltmp" builder
    | Let_in { ident; typ; binding; body; _ } ->
      let pointer = build_alloca (codegen_type typ) ident builder in
      let binding = codegen_rvalue binding in
      ignore (build_store binding pointer builder : llvalue);
      Hashtbl.set names ~key:ident ~data:(Pointer pointer);
      codegen_rvalue body
  in

  let codegen_lvalue_name ident =
    match Hashtbl.find_exn names ident with
    | Pointer p -> p
    | Value _ -> assert false
  in

  let codegen_lvalue (expr: Expr.t) =
    match expr with
    | Name { ident; _ } -> codegen_lvalue_name ident
    | _ -> assert false
  in

  let codegen_stmt (stmt: Control_flow.Stmt.t) ~builder =
    match stmt with
    | Expr expr ->
      ignore (codegen_rvalue expr ~builder : llvalue)
    | Let { ident; binding; _ } ->
      let binding = codegen_rvalue binding ~builder in
      Hashtbl.set names ~key:ident ~data:(Value binding)
    | Var { ident; binding; typ; _ } ->
      let binding = codegen_rvalue binding ~builder in
      let pointer = build_alloca (codegen_type typ) ident builder in
      ignore (build_store binding pointer builder : llvalue);
      Hashtbl.set names ~key:ident ~data:(Pointer pointer)
    | Assign { dst; src; _ } ->
      let dst = codegen_lvalue dst in
      let src = codegen_rvalue src ~builder in
      ignore (build_store src dst builder : llvalue)
  in

  let exit = ref None in
  let cache = Srcloc.Table.create () in

  let rec codegen_flow (flow: Control_flow.Flow.t) ~func ~builder =
    match flow with
    | Exit
    | Return { loc = _; arg = None } ->
      ignore (build_ret_void builder : llvalue)
    | Return { loc = _; arg = Some arg } ->
      let arg = codegen_rvalue arg ~builder in
      ignore (build_ret arg builder : llvalue)
    | If { loc = _; cond; iftrue; iffalse } ->
      let cond = codegen_rvalue cond ~builder in
      let iftrue = codegen_block iftrue ~func ~name:"iftrue" in
      let iffalse = codegen_block iffalse ~func ~name:"iffalse" in
      ignore (build_cond_br cond iftrue iffalse builder : llvalue)
    | Seq (stmt, flow) ->
      codegen_stmt stmt ~builder;
      codegen_flow flow ~func ~builder
  and codegen_block (flow: Control_flow.Flow.t) ~func ~name =
    match flow, !exit with
    | Exit, None ->
      let block = append_block (module_context module_) "exit" func in
      let builder = builder_at_end (module_context module_) block in
      codegen_flow flow ~func ~builder;
      exit := Some block;
      block
    | Exit, Some exit -> exit
    | _ ->
      Hashtbl.find_or_add cache (Control_flow.Flow.loc_exn flow)
        ~default:begin fun () ->
          let block = append_block (module_context module_) name func in
          let builder = builder_at_end (module_context module_) block in
          codegen_flow flow ~func ~builder;
          block
        end
  in

  let ctor_type = codegen_type (Type.fun_ ~params:[] ~ret:Type.void) in

  let ctors = Stack.create () in

  let add_ctor ctor = Stack.push ctors ctor in

  let finish_ctors () =
    if Stack.is_empty ctors then ();
    let init_ctors_func = define_function "init.ctors" ctor_type module_ in
    let init_entry = entry_block init_ctors_func in
    let builder = builder_at_end (module_context module_) init_entry in
    Stack.until_empty ctors
      (fun ctor -> ignore (build_call ctor [||] "" builder : llvalue));
    ignore (build_ret_void builder : llvalue);
    let global_ctors = define_global "llvm.global_ctors"
        (const_array (pointer_type ctor_type) [|init_ctors_func|])
        module_ in
    set_linkage Linkage.Appending global_ctors
  in

  let rec codegen_decl (decl: Control_flow.Decl.t) =
    protect ~f:(fun () -> codegen_decl' decl)
      ~finally:(fun () -> exit := None)
  and codegen_decl' decl =
    match decl with
    | Let { loc = _; ident; typ; binding } ->
      let typ = codegen_type typ in
      let global = define_global ident (undef typ) module_ in
      let ctor_func = define_function ("init.ctors." ^ ident) ctor_type module_ in
      let ctor_entry = entry_block ctor_func in
      let ctor_builder = builder_at_end (module_context module_) ctor_entry in
      let binding = codegen_rvalue binding ~builder:ctor_builder in
      ignore (build_store binding global ctor_builder : llvalue);
      ignore (build_ret_void ctor_builder : llvalue);
      add_ctor ctor_func;
      Hashtbl.set names ~key:ident ~data:(Pointer global);
    | Fun { loc = _; ident; params; typ; body } ->
      (* Definition *)
      let func = define_function
          (String.chop_suffix_exn ~suffix:"!" ident)
          (codegen_type typ)
          module_ in
      List.iteri params ~f:begin fun i ident ->
        let value = param func i in
        set_value_name ident value;
        Hashtbl.set names ~key:ident ~data:(Value value);
      end;
      let body = codegen_block body ~func ~name:"body" in

      (* Branch from entry block to body block *)
      let entry = entry_block func in
      let entry_builder = builder_at_end (module_context module_) entry in
      ignore (build_br body entry_builder : llvalue);

      (* Check for block terminators *)
      begin
        let implicit_return = Type.equal (Type.ret_exn typ) Type.void in
        Array.iter (basic_blocks func)
          ~f:begin fun block ->
            match block_terminator block with
            | Some _ -> ()
            | None ->
              if implicit_return then begin
                let builder = builder_at_end (module_context module_) block in
                ignore (build_ret_void builder : llvalue)
              end else
                failwith "Missing return in non-void function"
          end
      end;

      (* Declare *)
      Hashtbl.set names ~key:ident ~data:(Value func);
  in
  List.iter cf ~f:codegen_decl;
  finish_ctors ()

let codegen_ast m ast =
  let semantic = Semantic_analysis.analyze_ast ast in
  codegen_cf m @@ Control_flow.of_semantic semantic
