exception Type_error of {
    loc: Srcloc.t;
    expected: Type.t list;
    got: Type.t;
  }

exception Arity_mismatch of {
    loc: Srcloc.t;
    expected: int;
    got: int;
  }

exception Purity_error of {
    loc: Srcloc.t
  }

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
    | Assign of {
        loc: Srcloc.t;
        dst: t;
        src: t;
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
    | Var_in of {
        loc: Srcloc.t;
        ident: string;
        binding: t;
        body: t;
      }
  [@@deriving sexp_of, variants]

  val loc : t -> Srcloc.t

  val typ : t -> Type.t

  val impurities : t -> String.Set.t

  val is_pure : t -> bool

  val int : loc:Srcloc.t -> value:int64 -> t

  val bool : loc:Srcloc.t -> value:bool -> t

  val float : loc:Srcloc.t -> value:float -> t

  val name : loc:Srcloc.t -> ident:string -> typ:Type.t -> pure:bool -> t

  val binop : loc:Srcloc.t -> op:Bop.t -> lhs:t -> rhs:t -> t

  val assign : loc:Srcloc.t -> src:t -> dst:t -> t

  val call : loc:Srcloc.t -> callee:t -> args:t list -> t

  val let_in : ?binding_type:Type.t -> loc:Srcloc.t -> ident:string -> binding:t -> body:t -> t

  val var_in : ?binding_type:Type.t -> loc:Srcloc.t -> ident:string -> binding:t -> body:t -> t
end

module Stmt : sig
  type t = private
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

  val let_ : loc:Srcloc.t -> typ:Type.t -> string -> Expr.t -> t

  val var : loc:Srcloc.t -> typ:Type.t -> string -> Expr.t -> t

  val to_block : t -> t
end

module Decl : sig
  type t = private
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

  val let_ : loc:Srcloc.t -> ident:string -> typ:Type.t -> binding:Expr.t -> t

  val typ : t -> Type.t
end

type t = Decl.t list
