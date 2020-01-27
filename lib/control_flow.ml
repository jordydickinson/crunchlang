module Type_expr = Ast.Type_expr

module Expr = Ast.Expr

module Stmt = struct
  type t =
    | Expr of Expr.t
    | Let of {
        loc: Srcloc.t;
        ident: string;
        typ: Type_expr.t option [@sexp.option];
        binding: Expr.t;
      }
    | Var of {
        loc: Srcloc.t;
        ident: string;
        typ: Type_expr.t option [@sexp.option];
        binding: Expr.t;
      }
    | Assign of {
        loc: Srcloc.t;
        dst: Expr.t;
        src: Expr.t;
      }
  [@@deriving sexp_of, variants]

  let of_ast_stmt (stmt: Ast.Stmt.t) =
    match stmt with
    | Expr expr -> Expr expr
    | Let { loc; ident; typ; binding } ->
      Let { loc; ident; typ; binding }
    | Var { loc; ident; typ; binding } ->
      Var { loc; ident; typ; binding }
    | Assign { loc; dst; src } ->
      Assign { loc; dst; src }
    | If _
    | Return _
    | Block _ -> assert false
end

module Flow = struct
  type t =
    | Return of {
        loc: Srcloc.t;
        arg: Expr.t option [@sexp.option]
      }
    | If of {
        loc: Srcloc.t;
        cond: Expr.t;
        iftrue: t;
        iffalse: t;
      }
    | Seq of Stmt.t * t
  [@@deriving sexp_of, variants]

  let rec of_ast_stmts (stmts: Ast.Stmt.t list) ~continue =
    match stmts with
    | [] -> continue
    | (Expr _ as stmt) :: stmts
    | (Let _ as stmt) :: stmts
    | (Var _ as stmt) :: stmts
    | (Assign _ as stmt) :: stmts ->
      let stmt = Stmt.of_ast_stmt stmt in
      Seq (stmt, of_ast_stmts stmts ~continue)
    | If { loc; cond; iftrue; iffalse } :: stmts ->
      let to_stmts (stmt: Ast.Stmt.t) =
        match stmt with
        | Block stmts -> stmts
        | _ -> [stmt] in
      let continue = of_ast_stmts stmts ~continue in
      If {
        loc; cond;
        iftrue = of_ast_stmts (to_stmts iftrue) ~continue;
        iffalse =
          Option.value_map ~default:continue
            iffalse ~f:(Fn.compose (of_ast_stmts ~continue) to_stmts)
      }
    | Return { loc; arg } :: stmts ->
      if Fn.non List.is_empty stmts
      then failwith "Statements cannot appear after return"
      else Return { loc; arg }
    | Block stmts  :: stmts' ->
      let continue = of_ast_stmts stmts' ~continue in
      of_ast_stmts stmts ~continue
end

module Decl = struct
  type t =
    | Fun of {
        loc: Srcloc.t;
        name: string;
        params: (string * Type_expr.t) list;
        ret_type: Type_expr.t;
        body: Flow.t;
      }
  [@@deriving sexp_of, variants]
end

type t = Decl.t list
