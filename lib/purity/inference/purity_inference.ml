module Bop = Purity.Bop
module Expr = Purity.Expr
module Pure_expr = Purity.Pure_expr
module Stmt = Purity.Stmt
module Decl = Purity.Decl

module Env = Purity_env

let infer (ast: Ast.t): Purity.t Or_error.t =
  let open Or_error.Let_syntax in

  let env = Env.create () in

  let rec of_ast_decl (decl: Ast.Decl.t): Decl.t Or_error.t =
    let%map ident, decl =
      match decl with
      | Fun { loc; ident; params; ret_type; body } ->
        Env.scoped env ~f:begin fun () ->
          let params, param_types =
            List.map params ~f:begin fun (ident, typ) ->
              let typ = Type.of_type_expr typ in
              Env.bind env ~ident ~typ ~pure:true;
              ident, typ
            end
            |> List.unzip in
          let ret_type = Type.of_type_expr ret_type in
          Env.bind env
            ~ident:"return"
            ~typ:ret_type
            ~pure:false;
          let typ = Type.fun_ ~params:param_types ~ret:ret_type in
          let%map body = of_ast_stmt body in
          ident, Decl.fun_ ~loc ~ident ~params ~typ ~body
        end
    in
    Env.bind env ~ident ~typ:(Decl.typ decl) ~pure:false;
    decl

  and of_ast_stmt (stmt: Ast.Stmt.t): Stmt.t Or_error.t =
    match stmt with
    | Expr expr ->
      let%map expr = of_ast_expr expr in
      Stmt.expr expr
    | Block stmts ->
      List.map stmts ~f:of_ast_stmt
      |> Or_error.all
      >>| Stmt.block
    | Let { loc; ident; typ; binding } ->
      let%bind binding = pure_of_ast_expr binding in
      let binding_type = Pure_expr.typ binding in
      let typ = Option.value_map typ ~default:binding_type ~f:Type.of_type_expr in
      if not @@ Type.equal typ binding_type then
        error "Type error" stmt [%sexp_of: Ast.Stmt.t]
      else begin
        Env.bind env ~ident ~typ ~pure:true;
        return @@ Stmt.(let_) ~loc ~ident ~typ ~binding
      end
    | Var { loc; ident; typ; binding } ->
      let%bind binding = of_ast_expr binding in
      let binding_type = Expr.typ binding in
      let typ = Option.value_map typ ~default:binding_type ~f:Type.of_type_expr in
      if not @@ Type.equal typ binding_type then
        error "Type error" stmt [%sexp_of: Ast.Stmt.t]
      else begin
        Env.bind env ~ident ~typ ~pure:false;
        return @@ Stmt.var ~loc ~ident ~typ ~binding
      end
    | Assign { loc; dst; src } ->
      let%bind dst = of_ast_expr dst in
      let%bind src = of_ast_expr src in
      if not @@ Type.equal (Expr.typ dst) (Expr.typ src) then
        error "Type error" stmt [%sexp_of: Ast.Stmt.t]
      else
        return @@ Stmt.assign ~loc ~dst ~src
    | If { loc; cond; iftrue; iffalse } ->
      let%bind cond = of_ast_expr cond in
      if not @@ Type.equal Type.bool (Expr.typ cond) then
        error "Type error" stmt [%sexp_of: Ast.Stmt.t]
      else
        let%bind iftrue = of_ast_stmt iftrue in
        let%map iffalse =
          match iffalse with
          | None -> Ok None
          | Some iffalse ->
            let%map iffalse = of_ast_stmt iffalse in
            Some iffalse
        in
        Stmt.if_ ~loc ~cond ~iftrue ~iffalse
    | Return { loc; arg; } ->
      let%bind { typ = ret_type; _ } = Env.lookup env "return" in
      let%map arg =
        match arg with
        | None ->
          if not @@ Type.equal ret_type Type.void then
            error "Type error" stmt [%sexp_of: Ast.Stmt.t]
          else
            Ok None
        | Some arg ->
          let%bind arg = of_ast_expr arg in
          if not @@ Type.equal ret_type @@ Expr.typ arg then
            error "Type error" stmt [%sexp_of: Ast.Stmt.t]
          else
            return @@ Some arg
      in
      Stmt.return ~loc ~arg

  and of_ast_expr (expr: Ast.Expr.t): Expr.t Or_error.t =
    let%map expr = of_ast_expr' expr in
    Expr.lift_pure_exprs expr

  and of_ast_expr' (expr: Ast.Expr.t): Expr.t Or_error.t =
    match expr with
    | Int { loc; value } -> return @@ Expr.pure @@ Pure_expr.int ~loc ~value
    | Bool { loc; value } -> return @@ Expr.pure @@ Pure_expr.bool ~loc ~value
    | Float { loc; value } -> return @@ Expr.pure @@ Pure_expr.float ~loc ~value
    | Name { loc; ident } -> of_ast_name ~loc ~ident
    | Binop { loc; op; lhs; rhs } -> of_ast_binop ~loc ~op ~lhs ~rhs
    | Call { loc; callee; args } -> of_ast_call ~loc ~callee ~args

  and of_ast_name ~loc ~ident =
    match%map Env.lookup env ident with
    | { typ; pure = true } ->
      Expr.pure @@ Pure_expr.name ~loc ~ident ~typ
    | { typ; pure = false } ->
      Expr.name ~loc ~ident ~typ

  and of_ast_binop ~loc ~op:Ast.Expr.Bop.Add ~lhs ~rhs =
    let%bind lhs = of_ast_expr lhs in
    let%bind rhs = of_ast_expr rhs in
    let%map op, typ =
      match Expr.typ lhs, Expr.typ rhs with
      | Int64, Int64 -> return (Bop.Add, Type.Int64)
      | Float, Float -> return (Bop.Fadd, Type.Float)
      | _ -> error "Type error" (loc, lhs, rhs)
               [%sexp_of: Srcloc.t * Expr.t * Expr.t]
    in
    Expr.binop ~loc ~op ~lhs ~rhs ~typ

  and of_ast_call ~loc ~callee ~args =
    let%bind callee = of_ast_expr callee in
    let callee_type = Expr.typ callee in
    let%bind args = List.map args ~f:of_ast_expr |> Or_error.all in
    let arg_types = List.map args ~f:Expr.typ in
    let%bind param_types = Type.params_or_error callee_type in
    if List.length param_types <> List.length arg_types then
      error "Arity mismatch" (loc, callee, args)
        [%sexp_of: Srcloc.t * Expr.t * Expr.t list]
    else if not @@ List.for_all2_exn ~f:Type.equal param_types arg_types then
      error "Type error" (loc, callee, args)
        [%sexp_of: Srcloc.t * Expr.t * Expr.t list]
    else
      return @@ Expr.call ~loc ~callee ~args ~typ:(Type.ret_exn callee_type)

  and pure_of_ast_expr (expr: Ast.Expr.t): Pure_expr.t Or_error.t =
    let%bind expr = of_ast_expr expr in
    if Fn.non Expr.is_pure expr then
      error "Impure" expr [%sexp_of: Expr.t]
    else
      return @@ Expr.pure_expr_exn expr

  in
  List.map ast ~f:of_ast_decl
  |> Or_error.all
