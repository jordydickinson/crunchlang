module Bop = Purity.Bop
module Pure_expr = Purity.Pure_expr
module Expr = Purity.Expr

module Stmt = struct
  type t =
    | Expr of Expr.t
    | Let of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: Pure_expr.t;
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

  let of_purity_stmt_exn (stmt: Purity.Stmt.t) =
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

  let rec of_purity_stmts (stmts: Purity.Stmt.t list) ~continue =
    match stmts with
    | [] -> continue
    | (Expr _ as stmt) :: stmts
    | (Let _ as stmt) :: stmts
    | (Var _ as stmt) :: stmts
    | (Assign _ as stmt) :: stmts ->
      let stmt = Stmt.of_purity_stmt_exn stmt in
      Seq (stmt, of_purity_stmts stmts ~continue)
    | If { loc; cond; iftrue; iffalse } :: stmts ->
      let to_stmts (stmt: Purity.Stmt.t) =
        match stmt with
        | Block stmts -> stmts
        | _ -> [stmt] in
      let continue = of_purity_stmts stmts ~continue in
      If {
        loc; cond;
        iftrue = of_purity_stmts (to_stmts iftrue) ~continue;
        iffalse =
          Option.value_map ~default:continue
            iffalse ~f:(Fn.compose (of_purity_stmts ~continue) to_stmts)
      }
    | Return { loc; arg } :: stmts ->
      if Fn.non List.is_empty stmts
      then failwith "Statements cannot appear after return"
      else Return { loc; arg }
    | Block stmts  :: stmts' ->
      let continue = of_purity_stmts stmts' ~continue in
      of_purity_stmts stmts ~continue

  let loc_exn = function
    | Exit -> invalid_arg "loc_exn Exit"
    | Return { loc; _ }
    | If { loc; _ } -> loc
    | Seq (stmt, _) -> Stmt.loc stmt
end

module Decl = struct
  module Fun = struct
    type t = {
      loc: Srcloc.t;
      ident: string;
      params: string list;
      typ: Type.t;
      body: Flow.t;
    }
    [@@deriving sexp_of, fields]
  end

  type t =
    | Fun of Fun.t
  [@@deriving sexp_of, variants]

  let rec of_purity_decl (decl: Purity.Decl.t) =
    match decl with
    | Fun { loc; ident; params; typ; body = Block stmts } ->
      Fun {
        loc; ident; params; typ;
        body = Flow.of_purity_stmts stmts ~continue:Exit;
      }
    | Fun { loc; ident; params; typ; body } ->
      of_purity_decl
      @@ Purity.Decl.fun_
        ~loc ~ident ~params ~typ
        ~body:(Purity.Stmt.to_block body)
end

type t = Decl.t list

let of_purity (purity: Purity.t) =
  List.map purity ~f:Decl.of_purity_decl