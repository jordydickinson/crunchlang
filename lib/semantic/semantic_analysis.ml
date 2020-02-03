module Bop = Semantic.Bop
module Expr = Semantic.Expr
module Stmt = Semantic.Stmt
module Decl = Semantic.Decl

module Env = Semantic_env

exception Unbound_identifier of {
    loc: Srcloc.t;
    ident: string;
  }

exception Type_error of {
    loc: Srcloc.t;
    expected: Type.t list;
    got: Type.t;
  }

exception Arity_mismatch of {
    loc: Srcloc.t;
    expected: int;
    got: int;
  }

let analyze_ast (ast: Ast.t): Semantic.t =
  let env = Env.create () in
  let ret_type = ref Type.void in

  let rec of_ast_decl (decl: Ast.Decl.t): Decl.t =
    match decl with
    | Let { loc; ident; typ; binding } ->
      let typ = Type.of_type_expr typ in
      let binding = of_ast_expr binding in
      Env.bind env ~ident ~typ ~pure:true;
      Decl.(let_) ~loc ~ident ~typ ~binding
    | Fun { loc; ident; params; ret_type = ret_type'; body } ->
      let typ, params, body =
        Env.scoped env ~f:begin fun () ->
          let params, param_types =
            List.map params ~f:begin fun (ident, typ) ->
              let typ = Type.of_type_expr typ in
              Env.bind env ~ident ~typ ~pure:true;
              ident, typ
            end
            |> List.unzip in
          let ret_type' = Type.of_type_expr ret_type' in
          ret_type := ret_type';
          let typ = Type.fun_ ~params:param_types ~ret:ret_type' in
          let body = of_ast_stmt body in
          typ, params, body
        end in
      Env.bind env ~ident ~typ ~pure:false;
      Decl.fun_ ~loc ~ident ~params ~typ ~body

  and of_ast_stmt (stmt: Ast.Stmt.t): Stmt.t =
    match stmt with
    | Expr expr -> Stmt.expr @@ of_ast_expr expr
    | Block stmts -> Stmt.block @@ List.map stmts ~f:of_ast_stmt
    | Let { loc; ident; typ; binding } ->
      let binding = of_ast_expr binding in
      let binding_type = Expr.typ binding in
      let typ = Option.value_map typ ~default:binding_type ~f:Type.of_type_expr in
      if not @@ Type.equal typ binding_type
      then raise @@ Type_error {
          loc = Expr.loc binding;
          expected = [typ];
          got = binding_type;
        };
      Env.bind env ~ident ~typ ~pure:true;
      Stmt.(let_) ~loc ~ident ~typ ~binding
    | Let_block { loc = _; ident = _; typ = _; body = _ } -> assert false
    | Var { loc; ident; typ; binding } ->
      let binding = of_ast_expr binding in
      let binding_type = Expr.typ binding in
      let typ = Option.value_map typ ~default:binding_type ~f:Type.of_type_expr in
      if not @@ Type.equal typ binding_type
      then raise @@ Type_error {
          loc = Expr.loc binding;
          expected = [typ];
          got = Expr.typ binding;
        };
      Env.bind env ~ident ~typ ~pure:false;
      Stmt.var ~loc ~ident ~typ ~binding
    | If { loc; cond; iftrue; iffalse } ->
      let cond = of_ast_expr cond in
      if not @@ Type.equal Type.bool (Expr.typ cond)
      then raise @@ Type_error {
          loc = Expr.loc cond;
          expected = [Type.bool];
          got = Expr.typ cond;
        };
      let iftrue = of_ast_stmt iftrue in
      let iffalse = Option.map iffalse ~f:of_ast_stmt in
      Stmt.if_ ~loc ~cond ~iftrue ~iffalse
    | Return { loc; arg } ->
      let arg = Option.map arg ~f:of_ast_expr in
      let arg_type = Option.value_map arg ~default:Type.void ~f:Expr.typ in
      if not @@ Type.equal !ret_type arg_type
      then raise @@ Type_error {
          loc;
          expected = [!ret_type];
          got = arg_type;
        };
      Stmt.return ~loc ~arg

  and of_ast_expr (expr: Ast.Expr.t): Expr.t =
    match expr with
    | Int { loc; value } -> Expr.int ~loc ~value
    | Bool { loc; value } -> Expr.bool ~loc ~value
    | Float { loc; value } -> Expr.float ~loc ~value
    | Name { loc; ident } -> of_ast_name ~loc ~ident
    | Binop { loc; op; lhs; rhs } -> of_ast_binop ~loc ~op ~lhs ~rhs
    | Assign { loc; dst; src } -> of_ast_assign ~loc ~dst ~src
    | Call { loc; callee; args } -> of_ast_call ~loc ~callee ~args
    | Let_in { loc; ident; typ; binding; body } -> of_ast_let_in ~loc ~ident ~typ ~binding ~body
    | Var_in { loc; ident; typ; binding; body } -> of_ast_var_in ~loc ~ident ~typ ~binding ~body

  and of_ast_name ~loc ~ident =
    match Env.lookup env ident with
    | None -> raise @@ Unbound_identifier { loc; ident }
    | Some { typ; pure = true } ->
      Expr.name ~loc ~ident ~typ ~pure:true
    | Some { typ; pure = false } ->
      Expr.name ~loc ~ident ~typ ~pure:false

  and of_ast_binop ~loc ~op:Ast.Expr.Bop.Add ~lhs ~rhs =
    let lhs = of_ast_expr lhs in
    let rhs = of_ast_expr rhs in
    let op, typ =
      match Expr.typ lhs, Expr.typ rhs with
      | Int64, Int64 -> Bop.Add, Type.int64
      | Float, Float -> Bop.Fadd, Type.float
      | Int64, _ -> raise @@ Type_error {
          loc = Expr.loc rhs;
          expected = [Type.int64];
          got = Expr.typ rhs;
        }
      | Float, _ -> raise @@ Type_error {
          loc = Expr.loc rhs;
          expected = [Type.float];
          got = Expr.typ rhs;
        }
      | _ -> raise @@ Type_error {
          loc = Expr.loc lhs;
          expected = [Type.int64; Type.float];
          got = Expr.typ lhs;
        }
    in
    Expr.binop ~loc ~op ~lhs ~rhs ~typ

  and of_ast_assign ~loc ~dst ~src =
    let dst = of_ast_expr dst in
    let src = of_ast_expr src in
    if not @@ Type.equal (Expr.typ dst) (Expr.typ src)
    then raise @@ Type_error {
        loc = Expr.loc src;
        expected = [Expr.typ dst];
        got = Expr.typ src;
      };
    Expr.assign ~loc ~dst ~src

  and of_ast_call ~loc ~callee ~args =
    let callee = of_ast_expr callee in
    let callee_type = Expr.typ callee in
    let args = List.map args ~f:of_ast_expr in
    let arg_types = List.map args ~f:Expr.typ in
    let param_types = Type.params_exn callee_type in
    if List.length param_types <> List.length arg_types
    then raise @@ Arity_mismatch {
        loc;
        expected = List.length param_types;
        got = List.length arg_types;
      };
    List.iter2_exn param_types args ~f:begin fun param_type arg ->
      if not @@ Type.equal param_type @@ Expr.typ arg
      then raise @@ Type_error {
          loc = Expr.loc arg;
          expected = [param_type];
          got = Expr.typ arg;
        };
    end;
    Expr.call ~loc ~callee ~args ~typ:(Type.ret_exn callee_type)

  and of_ast_let_in ~loc ~ident ~typ ~binding ~body =
    let binding = of_ast_expr binding in
    let binding_type = Option.value_map typ ~f:Type.of_type_expr ~default:(Expr.typ binding) in
    if not @@ Type.equal binding_type (Expr.typ binding)
    then raise @@ Type_error {
        loc = Expr.loc binding;
        expected = [binding_type];
        got = Expr.typ binding;
      };
    let body = Env.scoped env ~f:(fun () ->
        Env.bind env ~ident ~typ:binding_type ~pure:true;
        of_ast_expr body) in
    let typ = Expr.typ body in
    Expr.(let_in) ~loc ~ident ~typ ~binding ~body

  and of_ast_var_in ~loc ~ident ~typ ~binding ~body =
    let binding = of_ast_expr binding in
    let binding_type = Option.value_map typ ~f:Type.of_type_expr ~default:(Expr.typ binding) in
    if not @@ Type.equal binding_type (Expr.typ binding)
    then raise @@ Type_error {
        loc = Expr.loc binding;
        expected = [binding_type];
        got = Expr.typ binding;
      };
    let body = Env.scoped env ~f:(fun () ->
        Env.bind env ~ident ~typ:binding_type ~pure:false;
        of_ast_expr body) in
    let typ = Expr.typ body in
    Expr.var_in ~loc ~ident ~typ ~binding ~body

  in
  List.map ast ~f:of_ast_decl
