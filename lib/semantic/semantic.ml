exception Purity_error of {
    loc: Srcloc.t
  }

module Bop = struct
  type t =
    | Add
    | Fadd
  [@@deriving sexp_of, variants]
end

module Expr = struct
  type t =
    | Int of {
        loc: Srcloc.t;
        value: int64;
      }
    | Bool of {
        loc: Srcloc.t;
        value: bool;
      }
    | Float of {
        loc: Srcloc.t;
        value: float;
      }
    | Name of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        pure: bool;
      }
    | Binop of {
        loc: Srcloc.t;
        op: Bop.t;
        lhs: t;
        rhs: t;
        typ: Type.t;
      }
    | Assign of {
        loc: Srcloc.t;
        dst: t;
        src: t;
      }
    | Call of {
        loc: Srcloc.t;
        callee: t;
        args: t list;
        typ: Type.t;
      }
    | Let_in of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: t;
        body: t;
      }
    | Var_in of {
        loc: Srcloc.t;
        ident: string;
        typ: Type.t;
        binding: t;
        body: t;
      }
  [@@deriving sexp_of, variants]

  let loc = function
    | Int { loc; _ }
    | Bool { loc; _ }
    | Float { loc; _ }
    | Name { loc; _ }
    | Binop { loc; _ }
    | Assign { loc; _ }
    | Call { loc; _ }
    | Let_in { loc; _ }
    | Var_in { loc; _ } -> loc

  let typ = function
    | Int _ -> Type.int64
    | Bool _ -> Type.bool
    | Float _ -> Type.float
    | Assign _ -> Type.void
    | Name { typ; _ }
    | Binop { typ; _ }
    | Call { typ; _ }
    | Let_in { typ; _ }
    | Var_in { typ; _ } -> typ

  let rec impurities = function
    | Int _ | Bool _ | Float _ -> String.Set.empty
    | Name { ident; pure; _ } ->
      if pure then String.Set.empty else String.Set.singleton ident
    | Binop { lhs; rhs; _ } -> Set.union (impurities lhs) (impurities rhs)
    | Assign { dst; src; _ } -> Set.union (impurities dst) (impurities src)
    | Call { callee; args; _ } ->
      String.Set.union_list (impurities callee :: List.map ~f:impurities args)
    | Let_in { body; _ } -> impurities body
    | Var_in { ident; binding; body; _ } ->
      Set.remove (Set.union (impurities binding) (impurities body)) ident

  let is_pure expr = Set.is_empty @@ impurities expr

  let let_in ~loc:loc' ~ident ~typ ~binding ~body =
    if not @@ is_pure binding
    then raise @@ Purity_error { loc = loc binding };
    (let_in) ~loc:loc' ~ident ~typ ~binding ~body
end

module Stmt = struct
  type t =
    | Expr of Expr.t
    | Block of t list
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
    | If of {
        loc: Srcloc.t;
        cond: Expr.t;
        iftrue: t;
        iffalse: t option [@sexp.option];
      }
    | Return of {
        loc: Srcloc.t;
        arg: Expr.t option [@sexp.option];
      }
  [@@deriving sexp_of, variants]

  let to_block = function
    | Block _ as stmt -> stmt
    | stmt -> Block [stmt]

  let let_ ~loc ~ident ~typ ~binding =
    if Fn.non Expr.is_pure binding
    then raise @@ Purity_error { loc = Expr.loc binding };
    (let_) ~loc ~ident ~typ ~binding
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
        body: Stmt.t;
      }
  [@@deriving sexp_of, variants]

  let typ = function
    | Let { typ; _ } -> typ
    | Fun { typ; _ } -> typ

  let let_ ~loc ~ident ~typ ~binding =
    if Fn.non Expr.is_pure binding
    then raise @@ Purity_error { loc = Expr.loc binding };
    (let_) ~loc ~ident ~typ ~binding
end

type t = Decl.t list
