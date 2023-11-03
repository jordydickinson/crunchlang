[@@@warning "-16"]

module Expr = Semantic.Expr
module Bop = Expr.Bop

module Stmt = struct
  type t =
    | Expr of Expr.t
    | Let of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Var of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Assign of {
        loc: Srcloc.t option [@sexp.option];
        dst: Expr.t;
        src: Expr.t;
      }
  [@@deriving sexp_of, variants]

  let of_semantic_stmt_exn (stmt: Semantic.Stmt.t) =
    match stmt with
    | Expr expr -> Expr expr
    | Assign { loc; dst; src } -> assign ~loc ~dst ~src
    | Let { loc; ident; typ; binding } -> (let_) ~loc ~ident ~typ ~binding
    | Var { loc; ident; typ; binding } -> var ~loc ~ident ~typ ~binding
    | Block _ | If _ | While _ | Return _ -> invalid_arg "Cannot be converted"

  let loc = function
    | Expr expr -> Expr.loc expr
    | Let { loc; _ }
    | Var { loc; _ }
    | Assign { loc; _ } -> loc
end

module Flow = struct
  type t =
    | Return of {
        loc: Srcloc.t option [@sexp.option];
        arg: Expr.t option [@sexp.option]
      }
    | If of {
        id: int;
        loc: Srcloc.t option [@sexp.option];
        cond: Expr.t;
        iftrue: t;
        iffalse: t;
      }
    | Break of { loc: Srcloc.t option [@sexp.option] }
    | Continue of { loc: Srcloc.t option [@sexp.option] }
    | Loop of {
        id: int;
        loc: Srcloc.t option [@sexp.option];
        entry: t;
        exit: t;
      }
    | Seq of { id: int; hd: Stmt.t; tl: t }
  [@@deriving sexp_of]

  let genid =
    let ctr = ref 0 in
    fun () -> incr ctr; !ctr

  let return ?loc arg = Return { loc; arg }
  let if_ ?loc cond ~iftrue ~iffalse =
    If { id = genid (); loc; cond; iftrue; iffalse }
  let break ?loc () = Break { loc }
  let continue ?loc () = Continue { loc }
  let loop ?loc ~entry ~exit = Loop { id = genid (); loc; entry; exit }
  let seq hd tl = Seq { id = genid (); hd; tl }

  let rec of_semantic_block (stmt: Semantic.Stmt.t) ~next =
    match stmt with
    | Block stmts -> of_semantic_stmts stmts ~next
    | _ -> of_semantic_block ~next @@ Semantic.Stmt.to_block stmt

  and of_semantic_stmts (stmts: Semantic.Stmt.t list) ~next =
    match stmts with
    | [] -> next
    | (Expr _ as stmt) :: stmts
    | (Let _ as stmt) :: stmts
    | (Var _ as stmt) :: stmts
    | (Assign _ as stmt) :: stmts ->
      let stmt = Stmt.of_semantic_stmt_exn stmt in
      seq stmt @@ of_semantic_stmts stmts ~next
    | If { loc; cond; iftrue; iffalse } :: stmts ->
      let continue = of_semantic_stmts stmts ~next in
      if_ ?loc cond
        ~iftrue:(of_semantic_block iftrue ~next)
        ~iffalse:(Option.value_map iffalse ~default:continue ~f:(of_semantic_block ~next))
    | While { loc; cond; body } :: stmts ->
      (* while cond body
         ->
         loop { if cond { body; continue; } else { break; } }
      *)
      loop ?loc
        ~entry:(if_ cond
                  ~iftrue:(of_semantic_block body ~next:(continue ()))
                  ~iffalse:(break ()))
        ~exit:(of_semantic_stmts stmts ~next)
    | Return { loc; arg } :: stmts ->
      if Fn.non List.is_empty stmts
      then failwith "Statements cannot appear after return"
      else return ?loc arg
    | Block stmts  :: stmts' ->
      let next = of_semantic_stmts stmts' ~next in
      of_semantic_stmts stmts ~next

  let id = function
    | Return _ | Break _ | Continue _ -> None
    | If { id; _ } | Loop { id; _ } | Seq { id; _ } -> Some id
end

module Decl = struct
  type t =
    | Type of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        binding: Type.t;
      }
    | Let of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type.t;
        binding: Expr.t;
      }
    | Fun of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        params: string list;
        typ: Type.t;
        body: Flow.t;
        pure: bool;
      }
    | Fun_expr of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        params: string list;
        typ: Type.t;
        body: Expr.t;
      }
    | Fun_extern of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        params: string list;
        typ: Type.t;
        pure: bool [@sexp.bool];
        extern_abi: string;
        extern_ident: string;
      }
  [@@deriving sexp_of]

  let of_semantic_decl (decl: Semantic.Decl.t) =
    match decl with
    | Type { loc; ident; binding } ->
      Type { loc; ident; binding }
    | Let { loc; ident; typ; binding } ->
      Let { loc; ident; typ; binding }
    | Fun { loc; ident; params; typ; body; pure } ->
      Fun {
        loc; ident; params; typ; pure;
        body = Flow.of_semantic_block body ~next:(Flow.return None);
      }
    | Fun_expr { loc; ident; params; typ; body } ->
      Fun_expr { loc; ident; params; typ; body }
    | Fun_extern { loc; ident; params; typ; pure; extern_abi; extern_ident } ->
      Fun_extern { loc; ident; params; typ; pure; extern_abi; extern_ident }

end

type t = Decl.t list

let of_semantic (semantic: Semantic.t) =
  List.map semantic ~f:Decl.of_semantic_decl
