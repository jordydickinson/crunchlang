exception Unbound_identifier of {
    loc: Srcloc.t;
    ident: string;
  }
[@@deriving sexp]

exception Type_error of {
    loc: Srcloc.t;
    expected: [ `Type of Type.t | `Kind of Type.Kind.t ];
    got: Type.t;
  }
[@@deriving sexp]

exception Unbound_type of {
    loc: Srcloc.t;
    ident: string;
  }
[@@deriving sexp]

exception Arity_mismatch of {
    loc: Srcloc.t;
    expected: int;
    got: int;
  }
[@@deriving sexp]

exception Purity_error of {
    loc: Srcloc.t
  }
[@@deriving sexp]

exception Not_assignable of {
    loc: Srcloc.t;
  }
[@@deriving sexp]

module Env : sig
  type t

  val empty : t
  val prelude : t
end

module Type : sig
  type t = Type.t
end

module Expr : sig
  module Bop = Ast.Expr.Bop

  type t = private
    | Int of {
        loc: Srcloc.t;
        value: int64
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
    | Deref of {
        loc: Srcloc.t;
        arg: t;
        typ: Type.t;
        pure: bool;
      }
    | Addr_of of {
        loc: Srcloc.t;
        arg: t;
        typ: Type.t;
      }
    | Array of {
        loc: Srcloc.t;
        elts: t array;
        elt_type: Type.t;
      }
    | Binop of {
        loc: Srcloc.t;
        op: Bop.t;
        lhs: t;
        rhs: t;
      }
    | Call of {
        loc: Srcloc.t;
        callee: t;
        args: t list;
      }
    | Let_in of {
        loc: Srcloc.t;
        ident: string;
        binding: t;
        body: t;
      }
  [@@deriving sexp_of]

  val loc : t -> Srcloc.t

  val typ : t -> Type.t

  val impurities : t -> String.Set.t

  val is_pure : t -> bool
end

module Stmt : sig
  type t = private
    | Expr of Expr.t
    | Block of t list
    | Assign of {
        loc: Srcloc.t;
        dst: Expr.t;
        src: Expr.t;
      }
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
  [@@deriving sexp_of]

  val to_block : t -> t
end

module Decl : sig
  type t = private
    | Type of {
        loc: Srcloc.t;
        ident: string;
        binding: Type.t;
      }
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
        pure: bool;
      }
    | Fun_expr of {
        loc: Srcloc.t;
        ident: string;
        params: string list;
        typ: Type.t;
        body: Expr.t;
      }
  [@@deriving sexp_of]
end

type t = Decl.t list
[@@deriving sexp_of]

val build_ast : Ast.t -> Env.t -> t * Env.t
