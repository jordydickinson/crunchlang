open LLVM

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
  let module Expr = Control_flow.Expr in
  let module Bop = Expr.Bop in

  let names = String.Table.create () in

  let rec codegen_type (typ: Type.t) =
    let codegen_array_type (elt: Type.t) =
      let elts_type = pointer_type @@ codegen_type elt in
      let size_type = pointer_type @@ i32_type @@ module_context module_ in
      struct_type (module_context module_) [|size_type; elts_type|]
    in
    match typ with
    | Void -> void_type (module_context module_)
    | Bool -> i1_type (module_context module_)
    | Int { bitwidth; signed = _ } -> integer_type (module_context module_) bitwidth
    | Float -> double_type (module_context module_)
    | Array elt -> codegen_array_type elt
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
    | _ -> assert false
  in

  let codegen_rvalue_name ident ~builder =
    match Hashtbl.find_exn names ident with
    | Value v -> v
    | Pointer p -> build_load p ident builder
  in

  let rec codegen_rvalue (expr: Expr.t) ~builder =
    let codegen_rvalue = codegen_rvalue ~builder in
    let codegen_bop ~op:Bop.Add ~lhs ~rhs =
      let typ = Expr.typ lhs in
      let lhs = codegen_rvalue lhs in
      let rhs = codegen_rvalue rhs in
      match typ with
      | Int _ -> build_add lhs rhs "addtmp" builder
      | Float -> build_fadd lhs rhs "addtmp" builder
      | _ -> assert false
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
    | Coerce { typ = Int _ as typ; arg } ->
      let signed = Expr.typ arg |> Type.is_signed_exn in
      let arg = codegen_rvalue arg in
      let typ = codegen_type typ in
      (if signed then build_sext_or_bitcast else build_zext_or_bitcast)
        arg typ (value_name arg ^ ".coerced") builder
    | Coerce _ -> assert false
    | Deref _ -> codegen_lvalue expr ~builder
    | Addr_of { arg; _ } -> codegen_lvalue arg ~builder
    | Array { elts; elt_type; _ } ->
      let typ = codegen_type @@ Expr.typ expr in
      let size_type = i32_type @@ module_context module_ in
      let size_pointer = build_malloc size_type "arraysize" builder in
      let size = const_int size_type (Array.length elts) in
      ignore (build_store size size_pointer builder : llvalue);
      let elt_type = codegen_type elt_type in
      let elts_pointer = build_array_malloc elt_type size "arrayelts" builder in
      let elts = const_array elt_type @@ Array.map elts ~f:codegen_rvalue in
      ignore (build_store elts elts_pointer builder : llvalue);
      let box_pointer = build_malloc typ "arraybox" builder in
      let box = const_struct (module_context module_) [|size_pointer; elts_pointer|] in
      ignore (build_store box box_pointer builder : llvalue);
      box_pointer
    | Binop { op; lhs; rhs; _ } -> codegen_bop ~op ~lhs ~rhs
    | Call { callee; args; _ } ->
      let name = if Type.equal Type.void @@ Type.ret_exn @@ Expr.typ callee
        then "" else "calltmp" in
      let callee = codegen_rvalue callee in
      let args = Array.of_list_map args ~f:codegen_rvalue in
      build_call callee args name builder
    | Let_in { ident; binding; body; _ } ->
      let binding = codegen_rvalue binding in
      Hashtbl.set names ~key:ident ~data:(Value binding);
      codegen_rvalue body
  in

  let codegen_stmt (stmt: Control_flow.Stmt.t) ~builder =
    match stmt with
    | Expr expr ->
      ignore (codegen_rvalue expr ~builder : llvalue)
    | Let { ident; binding; _ } ->
      let binding = codegen_rvalue binding ~builder in
      Hashtbl.set names ~key:ident ~data:(Value binding)
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

  let rec codegen_decl (decl: Control_flow.Decl.t) =
    protect ~f:(fun () -> codegen_decl' decl)
      ~finally:(fun () -> exit := None)
  and codegen_decl' decl =
    match decl with
    | Type _ -> ()
    | Fun_extern { ident; typ; extern_abi; extern_ident; _ } ->
      let abi = match extern_abi with
        | "C" -> CallConv.c
        | _ -> assert false in
      let extern = declare_function extern_ident (codegen_type typ) module_ in
      set_function_call_conv abi extern;
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
      ignore (build_ret value builder : llvalue)
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
  let semantic, _ = Semantic.build_ast ast Semantic.Env.prelude in
  codegen_cf m @@ Control_flow.of_semantic semantic
