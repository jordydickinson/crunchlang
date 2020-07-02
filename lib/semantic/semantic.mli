exception Unbound_identifier of {
    loc: Srcloc.t option [@sexp.option];
    ident: string;
  }
[@@deriving sexp]

exception Type_error of {
    loc: Srcloc.t option [@sexp.option];
    expected: [ `Type of Type.t | `Kind of Type.Kind.t ];
    got: Type.t;
  }
[@@deriving sexp]

exception Unbound_type of {
    loc: Srcloc.t option [@sexp.option];
    ident: string;
  }
[@@deriving sexp]

exception Arity_mismatch of {
    loc: Srcloc.t option [@sexp.option];
    expected: int;
    got: int;
  }
[@@deriving sexp]

exception Purity_error of {
    loc: Srcloc.t
  }
[@@deriving sexp]

exception Not_assignable of {
    loc: Srcloc.t option [@sexp.option];
  }
[@@deriving sexp]

exception Invalid_abi of {
    loc: Srcloc.t option [@sexp.option];
    ident: string;
  }
[@@deriving sexp]

exception Coercion_error of {
    loc: Srcloc.t option [@sexp.option];
    dst_type: Type.t;
    src_type: Type.t;
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
        loc: Srcloc.t option [@sexp.option];
        value: int64;
        typ: Type.t;
      }
    | Bool of {
        loc: Srcloc.t option [@sexp.option];
        value: bool;
      }
    | Float of {
        loc: Srcloc.t option [@sexp.option];
        value: float;
        typ: Type.t;
      }
    | Name of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type.t;
        pure: bool;
      }
    | Cast of {
        loc: Srcloc.t option;
        typ: Type.t;
        arg: t;
      }
    | Deref of t
    | Addr_of of t
    | Array of {
        loc: Srcloc.t option [@sexp.option];
        elts: t array;
        typ: Type.t;
      }
    | Subscript of {
        loc: Srcloc.t option [@sexp.option];
        arg: t;
        idx: t;
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
        binding: t;
        body: t;
      }
  [@@deriving sexp_of]

  val loc : t -> Srcloc.t option
  val loc_exn : t -> Srcloc.t

  val typ : t -> Type.t

  val impurities : t -> String.Set.t

  val is_pure : t -> bool
end

module Stmt : sig
  type t = private
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
        typ: Type.t;
        binding: Expr.t;
      }
    | Var of {
        loc: Srcloc.t option [@sexp.option];
        ident: string;
        typ: Type.t;
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

  val to_block : t -> t
end

module Decl : sig
  type t = private
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
        body: Stmt.t;
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
        pure: bool;
        extern_abi: string;
        extern_ident: string;
      }
  [@@deriving sexp_of]
end

type t = Decl.t list
[@@deriving sexp_of]

val build_ast : Ast.t -> Env.t -> t * Env.t
