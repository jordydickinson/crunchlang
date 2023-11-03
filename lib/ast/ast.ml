[@@@warning "-16"]

module Type_expr = struct
  type t =
    | Name of { loc: Srcloc.t option [@sexp.option]; ident: string }
    | Reference of { loc: Srcloc.t option [@sexp.option]; arg: t }
    | Array of { loc: Srcloc.t option [@sexp.option]; arg: t }
    | Struct of { loc: Srcloc.t option [@sexp.option]; fields: (string * t) list }
  [@@deriving sexp_of, variants]

  (* This is not a place of honor... *)
  let name ?loc ~ident = Name { loc; ident }
  let reference ?loc ~arg = Reference { loc; arg }
  let array ?loc ~arg = Array { loc; arg }
  let struct_ ?loc ~fields = Struct { loc; fields }
end

module Expr = struct
  module Bop = struct
    type t =
      | Add
      | Lt
    [@@deriving sexp_of]
  end

  type t =
    | Int of {
        loc: Srcloc.t option [@sexp.option];
        value: int64
      }
    | Bool of {
        loc: Srcloc.t option [@sexp.option];
        value: bool;
      }
    | Float of {
        loc: Srcloc.t option [@sexp.option];
        value: float;
      }
    | Name of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
      }
    | Array of {
        loc: Srcloc.t option [@sexp.option];
        elts: t array;
      }
    | Subscript of {
        loc: Srcloc.t option [@sexp.option];
        arg: t;
        idx: t;
      }
    | Cast of {
        loc: Srcloc.t option [@sexp.option];
        typ: Type_expr.t;
        arg: t;
      }
    | Binop of {
        loc: Srcloc.t option [@sexp.option];
        op: Bop.t;
        lhs: t;
        rhs: t;
      }
    | Call of {
        loc: Srcloc.t option [@sexp.option];
        callee: t;
        args: t list;
      }
    | Let_in of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type_expr.t option [@sexp.option];
        binding: t;
        body: t;
      }
  [@@deriving sexp_of]

  (* ... No highly-esteemed deed is commemorated here... *)
  let int ?loc ~value = Int { loc; value }
  let bool ?loc ~value = Bool { loc; value }
  let float ?loc ~value = Float { loc; value }
  let string ?loc:_ ~value:_ = assert false
  let name ?loc ~ident = Name { loc; ident }
  let array ?loc ~elts = Array { loc; elts }
  let subscript ?loc ~arg ~idx = Subscript { loc; arg; idx }
  let cast ?loc ~typ ~arg = Cast { loc; typ; arg }
  let binop ?loc ~op ~lhs ~rhs = Binop { loc; op; lhs; rhs }
  let call ?loc ~callee ~args = Call { loc; callee; args }
  let let_in ?loc ~ident ~typ ~binding ~body = Let_in { loc; ident; typ; binding; body }

  let add = binop ~op:Bop.Add
  let lt = binop ~op:Bop.Lt

  let loc = function
    | Int { loc; _ }
    | Bool { loc; _ }
    | Float { loc; _ }
    | Name { loc; _ }
    | Array { loc; _ }
    | Subscript { loc; _ }
    | Cast { loc; _ }
    | Binop { loc; _ }
    | Call { loc; _ }
    | Let_in { loc; _ } -> loc
end

module Stmt = struct
  type t =
    | Expr of Expr.t
    | Block of t list
    | Assign of {
        loc: Srcloc.t option [@sexp.option];
        dst: Expr.t;
        src: Expr.t;
      }
    | Let of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type_expr.t option [@sexp.option];
        binding: Expr.t;
      }
    | Var of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type_expr.t option [@sexp.option];
        binding: Expr.t;
      }
    | If of {
        loc: Srcloc.t option [@sexp.option];
        cond: Expr.t;
        iftrue: t;
        iffalse: t option [@sexp.option];
      }
    | While of {
        loc: Srcloc.t option [@sexp.option];
        cond: Expr.t;
        body: t;
      }
    | Return of {
        loc: Srcloc.t option [@sexp.option];
        arg: Expr.t option [@sexp.option];
      }
  [@@deriving sexp_of]

  (* ... Nothing valued is here... *)
  let expr e = Expr e
  let block stmts = Block stmts
  let assign ?loc ~dst ~src = Assign { loc; dst; src }
  let let_ ?loc ~ident ~typ ~binding = Let { loc; ident; typ; binding }
  let var ?loc ~ident ~typ ~binding = Var { loc; ident; typ; binding }
  let if_ ?loc ~cond ~iftrue ~iffalse = If { loc; cond; iftrue; iffalse }
  let while_ ?loc ~cond ~body = While { loc; cond; body }
  let return ?loc ~arg = Return { loc; arg }

  let to_block stmt =
    match stmt with
    | Block _ -> stmt
    | _ -> Block [stmt]
end

module Decl = struct
  type t =
    | Type of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        binding: Type_expr.t;
      }
    | Let of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type_expr.t;
        binding: Expr.t;
      }
    | Fun of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        params: (string * Type_expr.t) list;
        ret_type: Type_expr.t option [@sexp.option];
        body: Stmt.t;
        pure: bool [@sexp.bool];
      }
    | Fun_expr of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        params: (string * Type_expr.t) list;
        ret_type: Type_expr.t;
        body: Expr.t;
      }
    | Fun_extern of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        params: (string * Type_expr.t) list;
        ret_type: Type_expr.t option;
        pure: bool [@sexp.bool];
        extern_abi: string;
        extern_ident: string;
      }
  [@@deriving sexp_of, variants]

  (* ... What is here was dangerous and repulsive to us. *)
  let type_ ?loc ~ident ~binding = Type { loc; ident; binding }
  let let_ ?loc ~ident ~typ ~binding = Let { loc; ident; typ; binding }
  let fun_ ?loc ~ident ~params ~ret_type ~body ~pure = Fun { loc; ident; params; ret_type; body; pure }
  let fun_expr ?loc ~ident ~params ~ret_type ~body = Fun_expr { loc; ident; params; ret_type; body }
  let fun_extern ?loc ~ident ~params ~ret_type ~pure ~extern_abi ~extern_ident =
    Fun_extern { loc; ident; params; ret_type; pure; extern_abi; extern_ident }

  let loc = function
    | Type { loc; _ }
    | Let { loc; _ }
    | Fun { loc; _ }
    | Fun_expr { loc; _ }
    | Fun_extern { loc; _ } -> loc
end

type t = Decl.t list
