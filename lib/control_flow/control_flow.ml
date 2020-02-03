module Bop = Semantic.Bop
module Expr = Semantic.Expr

module Stmt = struct
  type t =
    | Expr of Expr.t
    | Let of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Var of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Assign of {
        loc: Srcloc.t;
        dst: Expr.t;
        src: Expr.t;
      }
  [@@deriving sexp_of, variants]

  let of_semantic_stmt_exn (stmt: Semantic.Stmt.t) =
    match stmt with
    | Expr expr -> Expr expr
    | Let { loc; ident; typ; binding } -> (let_) ~loc ~ident ~typ ~binding
    | Var { loc; ident; typ; binding } -> var ~loc ~ident ~typ ~binding
    | Assign { loc; dst; src } -> assign ~loc ~dst ~src
    | Block _
    | If _
    | Return _ -> invalid_arg "Cannot be converted"

  let loc = function
    | Expr expr -> Expr.loc expr
    | Let { loc; _ }
    | Var { loc; _ }
    | Assign { loc; _ } -> loc
end

module Flow = struct
  type t =
    | Exit
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

  let rec of_semantic_stmts (stmts: Semantic.Stmt.t list) ~continue =
    match stmts with
    | [] -> continue
    | (Expr _ as stmt) :: stmts
    | (Let _ as stmt) :: stmts
    | (Var _ as stmt) :: stmts
    | (Assign _ as stmt) :: stmts ->
      let stmt = Stmt.of_semantic_stmt_exn stmt in
      Seq (stmt, of_semantic_stmts stmts ~continue)
    | If { loc; cond; iftrue; iffalse } :: stmts ->
      let to_stmts (stmt: Semantic.Stmt.t) =
        match stmt with
        | Block stmts -> stmts
        | _ -> [stmt] in
      let continue = of_semantic_stmts stmts ~continue in
      If {
        loc; cond;
        iftrue = of_semantic_stmts (to_stmts iftrue) ~continue;
        iffalse =
          Option.value_map ~default:continue
            iffalse ~f:(Fn.compose (of_semantic_stmts ~continue) to_stmts)
      }
    | Return { loc; arg } :: stmts ->
      if Fn.non List.is_empty stmts
      then failwith "Statements cannot appear after return"
      else Return { loc; arg }
    | Block stmts  :: stmts' ->
      let continue = of_semantic_stmts stmts' ~continue in
      of_semantic_stmts stmts ~continue

  let loc_exn = function
    | Exit -> invalid_arg "loc_exn Exit"
    | Return { loc; _ }
    | If { loc; _ } -> loc
    | Seq (stmt, _) -> Stmt.loc stmt
end

module Decl = struct
  type t =
    | Let of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Fun of {
        loc: Srcloc.t;
        ident: string;
        params: string list;
        typ: Type.t;
        body: Flow.t;
      }
  [@@deriving sexp_of, variants]

  let rec of_semantic_decl (decl: Semantic.Decl.t) =
    match decl with
    | Let { loc; ident; typ; binding } ->
      Let { loc; ident; typ; binding }
    | Fun { loc; ident; params; typ; body = Block stmts } ->
      Fun {
        loc; ident; params; typ;
        body = Flow.of_semantic_stmts stmts ~continue:Exit;
      }
    | Fun { loc; ident; params; typ; body } ->
      of_semantic_decl
      @@ Semantic.Decl.fun_
        ~loc ~ident ~params ~typ
        ~body:(Semantic.Stmt.to_block body)
end

type t = Decl.t list

let of_semantic (semantic: Semantic.t) =
  List.map semantic ~f:Decl.of_semantic_decl
