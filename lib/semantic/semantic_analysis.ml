module Expr = Semantic.Expr
module Bop = Expr.Bop
module Stmt = Semantic.Stmt
module Decl = Semantic.Decl

module Env = Semantic_env

exception Unbound_identifier of {
    loc: Srcloc.t;
    ident: string;
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
      let typ = Option.value_map typ ~f:Type.of_type_expr ~default:binding_type in
      Env.bind env ~ident ~typ:binding_type ~pure:true;
      Stmt.(let_) ~loc ~typ ident binding
    | Let_block { loc = _; ident = _; typ = _; body = _ } -> assert false
    | Var { loc; ident; typ; binding } ->
      let binding = of_ast_expr binding in
      let binding_type = Expr.typ binding in
      let typ = Option.value_map typ ~default:binding_type ~f:Type.of_type_expr in
      Env.bind env ~ident ~typ ~pure:false;
      Stmt.var ~loc ~typ ident binding
    | If { loc; cond; iftrue; iffalse } ->
      let cond = of_ast_expr cond in
      let iftrue = of_ast_stmt iftrue in
      let iffalse = Option.map iffalse ~f:of_ast_stmt in
      Stmt.if_ ~loc ~cond ~iftrue ~iffalse
    | Return { loc; arg } ->
      let arg = Option.map arg ~f:of_ast_expr in
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

  and of_ast_binop ~loc ~op ~lhs ~rhs =
    let lhs = of_ast_expr lhs in
    let rhs = of_ast_expr rhs in
    Expr.binop ~loc ~op ~lhs ~rhs

  and of_ast_assign ~loc ~dst ~src =
    let dst = of_ast_expr dst in
    let src = of_ast_expr src in
    Expr.assign ~loc ~dst ~src

  and of_ast_call ~loc ~callee ~args =
    let callee = of_ast_expr callee in
    let args = List.map args ~f:of_ast_expr in
    Expr.call ~loc ~callee ~args

  and of_ast_let_in ~loc ~ident ~typ ~binding ~body =
    let binding = of_ast_expr binding in
    let binding_type = Option.map typ ~f:Type.of_type_expr in
    let body = Env.scoped env ~f:(fun () ->
        Env.bind env ~ident ~typ:(Expr.typ binding) ~pure:true;
        of_ast_expr body) in
    Expr.(let_in) ~loc ~ident ~binding ?binding_type ~body

  and of_ast_var_in ~loc ~ident ~typ ~binding ~body =
    let binding = of_ast_expr binding in
    let binding_type = Option.map typ ~f:Type.of_type_expr in
    let body = Env.scoped env ~f:(fun () ->
        Env.bind env ~ident ~typ:(Expr.typ binding) ~pure:false;
        of_ast_expr body) in
    Expr.var_in ~loc ~ident ~binding ?binding_type ~body

  in
  List.map ast ~f:of_ast_decl
