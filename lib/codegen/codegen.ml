open LLVM

let init = lazy (Llvm_all_backends.initialize ())

let global_context = LLVM.global_context

let create_context = LLVM.create_context

let create_module = LLVM.create_module

let dump_module = dump_module

let string_of_llmodule = string_of_llmodule

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
  let module Expr = Control_flow.Expr in
  let module Bop = Expr.Bop in

  let names = String.Table.create () in

  let rec codegen_type (typ: Type.t) =
    match typ with
    | Void -> void_type (module_context module_)
    | Bool -> i1_type (module_context module_)
    | Int { bitwidth; signed = _ } -> integer_type (module_context module_) bitwidth
    | Float32 -> float_type (module_context module_)
    | Float64 -> double_type (module_context module_)
    | Array { elt; size } -> array_type (codegen_type elt) size
    | Struct _ -> assert false
    | Pointer typ -> pointer_type @@ codegen_type typ
    | Fun { params; ret } ->
      function_type (codegen_type ret)
      @@ Array.of_list_map params ~f:codegen_type
  in

  let codegen_lvalue_name ident =
    match Hashtbl.find_exn names ident with
    | Pointer p -> p
    | Value _ -> assert false
  in

  let rec codegen_lvalue (expr: Expr.t) ~builder =
    match expr with
    | Name { ident; _ } -> codegen_lvalue_name ident
    | Deref { arg; _ } ->
      let ptr = codegen_lvalue arg ~builder in
      build_load ptr (value_name ptr ^ ".deref") builder
    | Subscript { arg; idx; _ } ->
      let arg = codegen_lvalue arg ~builder in
      let zero = const_int (codegen_type Type.int32) 0 in
      let idx = codegen_rvalue idx ~builder in
      build_gep arg [|zero; idx|] (value_name arg ^ ".i") builder
    | _ -> assert false
  and codegen_rvalue_name ident ~builder =
    match Hashtbl.find_exn names ident with
    | Value v -> v
    | Pointer p -> build_load p ident builder
  and codegen_rvalue (expr: Expr.t) ~builder =
    let codegen_rvalue = codegen_rvalue ~builder in
    let codegen_bop ~(op: Bop.t) ~lhs ~rhs =
      let typ = Expr.typ lhs in
      let lhs = codegen_rvalue lhs in
      let rhs = codegen_rvalue rhs in
      match op, typ with
      | Add, Int _ -> build_add lhs rhs "iaddtmp" builder
      | Add, Float64 -> build_fadd lhs rhs "faddtmp" builder
      | Add, _ -> assert false
      | Lt, Int { signed = true; _ } -> build_icmp Icmp.Slt lhs rhs "islttmp" builder
      | Lt, Int { signed = false; _ } -> build_icmp Icmp.Ult lhs rhs "iulttmp" builder
      | Lt, Float64 -> build_fcmp Fcmp.Ult lhs rhs "fulttmp" builder
      | Lt, _ -> assert false
    in
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
    | Cast { typ = Int _ as typ; arg; _ } ->
      let signed = Expr.typ arg |> Type.is_signed_exn in
      let arg = codegen_rvalue arg in
      let typ = codegen_type typ in
      (if signed then build_sext_or_bitcast else build_zext_or_bitcast)
        arg typ (value_name arg ^ ".coerced") builder
    | Cast { typ = Pointer _ as typ; arg; _ } when Type.is_kind (Expr.typ arg) Type.Kind.array ->
      let arg = codegen_lvalue arg ~builder in
      let zero = const_int (codegen_type Type.int32) 0 in
      let eltptr = build_gep arg [|zero; zero|] (value_name arg ^ ".0") builder in
      build_bitcast eltptr (codegen_type typ) (value_name eltptr ^ ".cast") builder
    | Cast _ -> assert false
    | Deref _ -> codegen_lvalue expr ~builder
    | Addr_of { arg; _ } -> codegen_lvalue arg ~builder
    | Array { elts; typ = Array { elt; _ }; _ } ->
      const_array (codegen_type elt) (Array.map elts ~f:codegen_rvalue)
    | Array _ -> assert false
    | Subscript _ ->
      let elt_pointer = codegen_lvalue expr ~builder in
      build_load elt_pointer (value_name elt_pointer ^ ".0") builder
    | Binop { op; lhs; rhs; _ } -> codegen_bop ~op ~lhs ~rhs
    | Call { callee; args; _ } ->
      let name = if Type.equal Type.void @@ Type.ret_exn @@ Expr.typ callee
        then "" else "calltmp" in
      let callee = codegen_rvalue callee in
      let args = Array.of_list_map args ~f:codegen_rvalue in
      build_call callee args name builder
    | Let_in { ident; binding; body; _ } ->
      let binding_type = codegen_type @@ Expr.typ binding in
      let pointer = build_alloca binding_type ident builder in
      let binding = codegen_rvalue binding in
      ignore (build_store binding pointer builder : llvalue);
      Hashtbl.set names ~key:ident ~data:(Pointer binding);
      codegen_rvalue body
  in

  let codegen_stmt (stmt: Control_flow.Stmt.t) ~builder =
    match stmt with
    | Expr expr ->
      ignore (codegen_rvalue expr ~builder : llvalue)
    | Let { ident; binding; typ; _ }
    | Var { ident; binding; typ; _ } ->
      let pointer = build_alloca (codegen_type typ) ident builder in
      let binding = codegen_rvalue binding ~builder in
      ignore (build_store binding pointer builder : llvalue);
      Hashtbl.set names ~key:ident ~data:(Pointer pointer)
    | Assign { dst; src; _ } ->
      let dst = codegen_lvalue dst ~builder in
      let src = codegen_rvalue src ~builder in
      ignore (build_store src dst builder : llvalue)
  in

  let loop_entry = ref None in
  let loop_exit = ref None in
  let cache = Int.Table.create () in

  let rec codegen_flow (flow: Control_flow.Flow.t) ~func ~builder =
    match flow with
    | Return { loc = _; arg = None } ->
      ignore (build_ret_void builder : llvalue)
    | Return { loc = _; arg = Some arg } ->
      let arg = codegen_rvalue arg ~builder in
      ignore (build_ret arg builder : llvalue)
    | If { cond; iftrue; iffalse; _ } ->
      let cond = codegen_rvalue cond ~builder in
      let iftrue = codegen_block iftrue ~func ~name:"iftrue" in
      let iffalse = codegen_block iffalse ~func ~name:"iffalse" in
      ignore (build_cond_br cond iftrue iffalse builder : llvalue)
    | Break { loc = _ } ->
      ignore (build_br (Option.value_exn !loop_exit) builder : llvalue)
    | Continue { loc = _ } ->
      ignore (build_br (Option.value_exn !loop_entry) builder : llvalue)
    | Loop { entry; exit; _ } ->
      let entry_block = append_block (module_context module_) "loop.entry" func in
      let exit_block = append_block (module_context module_) "loop.exit" func in
      let entry_builder = builder_at_end (module_context module_) entry_block in
      let exit_builder = builder_at_end (module_context module_) exit_block in
      loop_entry := Some entry_block;
      loop_exit := Some exit_block;
      codegen_flow entry ~func ~builder:entry_builder;
      codegen_flow exit ~func ~builder:exit_builder;
      ignore (build_br entry_block builder : llvalue)
    | Seq { hd = stmt; tl = flow; _ } ->
      codegen_stmt stmt ~builder;
      codegen_flow flow ~func ~builder
  and codegen_block (flow: Control_flow.Flow.t) ~func ~name =
    let build () =
      let block = append_block (module_context module_) name func in
      let builder = builder_at_end (module_context module_) block in
      codegen_flow flow ~func ~builder;
      block
    in match Control_flow.Flow.id flow with
    | None -> build ()
    | Some id -> Hashtbl.find_or_add cache id ~default:build
  in

  let rename_func ident ~pure =
    if pure
    then ident ^ "$pure"
    else String.chop_suffix_exn ident ~suffix:"!"
  in

  let ctor_type = codegen_type (Type.fun_ ~params:[] ~ret:Type.void) in

  let ctors = Stack.create () in

  let add_ctor ctor = Stack.push ctors ctor in

  let finish_ctors () =
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

  let codegen_decl (decl: Control_flow.Decl.t) =
    match decl with
    | Type _ -> ()
    | Fun_extern { ident; typ; extern_abi; extern_ident; _ } ->
      let abi = match extern_abi with
        | "C" -> CallConv.c
        | _ -> assert false in
      let extern = declare_function extern_ident (codegen_type typ) module_ in
      set_function_call_conv abi extern;
      Llvm_analysis.assert_valid_function extern;
      Hashtbl.set names ~key:ident ~data:(Value extern);
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
    | Fun_expr { ident; params; typ; body; _ } ->
      let func = define_function (rename_func ident ~pure:true) (codegen_type typ) module_ in
      Hashtbl.set names ~key:ident ~data:(Value func);
      List.iteri params ~f:begin fun i ident ->
        let value = param func i in
        set_value_name ident value;
        Hashtbl.set names ~key:ident ~data:(Value value);
      end;
      let entry = entry_block func in
      let builder = builder_at_end (module_context module_) entry in
      let value = codegen_rvalue body ~builder in
      ignore (build_ret value builder : llvalue);
      Llvm_analysis.assert_valid_function func;
    | Fun { ident; params; typ; body; pure; _ } ->
      (* Definition *)
      let func = define_function (rename_func ident ~pure) (codegen_type typ) module_ in
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

      (* Validate *)
      Llvm_analysis.assert_valid_function func;

      (* Declare *)
      Hashtbl.set names ~key:ident ~data:(Value func);
  in
  List.iter cf ~f:codegen_decl;
  finish_ctors ()

let codegen_ast m ast =
  let semantic, _ = Semantic.build_ast ast Semantic.Env.prelude in
  codegen_cf m @@ Control_flow.of_semantic semantic
