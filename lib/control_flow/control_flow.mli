module Bop = Semantic.Bop
module Pure_expr = Semantic.Pure_expr
module Expr = Semantic.Expr

module Stmt : sig

  type t = private
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

  val loc : t -> Srcloc.t
end

module Flow : sig
  type t = private
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

  (** [loc_exn flow] is the source location of [flow].
      @raise [Invalid_arg "loc_exn Exit"] [Exit] has no location. *)
  val loc_exn : t -> Srcloc.t
end

module Decl : sig
  module Fun : sig
    type t = private {
      loc: Srcloc.t;
      ident: string;
      params: string list;
      typ: Type.t;
      body: Flow.t;
    }
    [@@deriving sexp_of, fields]
  end

  type t = private
    | Fun of Fun.t
  [@@deriving sexp_of, variants]
end

type t = Decl.t list

val of_semantic : Semantic.t -> t
