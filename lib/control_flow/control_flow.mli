module Expr = Semantic.Expr

module Stmt : sig

  type t = private
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

  val loc : t -> Srcloc.t option
end

module Flow : sig
  type t = private
    | Exit
    | Return of {
        loc: Srcloc.t option [@sexp.option];
        arg: Expr.t option [@sexp.option]
      }
    | If of {
        loc: Srcloc.t option [@sexp.option];
        cond: Expr.t;
        iftrue: t;
        iffalse: t;
      }
    | Seq of Stmt.t * t
  [@@deriving sexp_of, variants]

  val loc_exn : t -> Srcloc.t
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
        pure: bool;
        extern_abi: string;
        extern_ident: string;
      }
  [@@deriving sexp_of, variants]
end

type t = Decl.t list

val of_semantic : Semantic.t -> t
