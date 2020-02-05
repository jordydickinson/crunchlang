exception Unbound_identifier of {
    loc: Srcloc.t;
    ident: string;
  }
[@@deriving sexp]

exception Type_error of {
    loc: Srcloc.t;
    expected: Type.t list;
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

module Env : sig
  type t

  val empty : t
end

module Type : sig
  type t = Type.t

  type builder = Env.t -> t
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

  type builder

  val loc : t -> Srcloc.t

  val typ : t -> Type.t

  val impurities : t -> String.Set.t

  val is_pure : t -> bool

  val int : loc:Srcloc.t -> value:int64 -> builder
  val bool : loc:Srcloc.t -> value:bool -> builder
  val float : loc:Srcloc.t -> value:float -> builder
  val name : loc:Srcloc.t -> ident:string -> builder
  val binop : loc:Srcloc.t -> op:Bop.t -> lhs:builder -> rhs:builder -> builder
  val call : loc:Srcloc.t -> callee:builder -> args:builder list -> builder
  val let_in : ?binding_type:Type.builder -> loc:Srcloc.t -> ident:string -> binding:builder -> body:builder -> builder
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

  type builder

  val expr : Expr.builder -> builder
  val block : builder list -> builder
  val assign : loc:Srcloc.t -> dst:Expr.builder -> src:Expr.builder -> builder
  val let_ : loc:Srcloc.t -> typ:Type.builder option -> ident:string -> binding:Expr.builder -> builder
  val var : loc:Srcloc.t -> typ:Type.builder option -> ident:string -> binding:Expr.builder -> builder
  val if_ : loc:Srcloc.t -> cond:Expr.builder -> iftrue:builder -> iffalse:builder option -> builder
  val return : loc:Srcloc.t -> arg:Expr.builder option -> builder

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

  type builder

  val let_ : loc:Srcloc.t -> ident:string -> typ:Type.builder -> binding:Expr.builder -> builder
  val fun_ : loc:Srcloc.t -> ident:string -> params:string list -> typ:Type.builder -> body:Stmt.builder -> pure:bool -> builder
end

type t = Decl.t list
[@@deriving sexp_of]

type builder

val declare : builder -> Decl.builder -> builder

val build : builder -> Env.t -> t * Env.t

val build_ast : Ast.t -> Env.t -> t * Env.t
