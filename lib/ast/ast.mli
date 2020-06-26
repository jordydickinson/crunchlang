module Type_expr : sig
  type t = private
    | Name of { loc: Srcloc.t option [@sexp.option]; ident: string }
    | Pointer of { loc: Srcloc.t option [@sexp.option]; arg: t }
    | Array of { loc: Srcloc.t option [@sexp.option]; arg: t }
    | Struct of { loc: Srcloc.t option [@sexp.option]; fields: (string * t) list }
  [@@deriving sexp_of]

  val name : ?loc:Srcloc.t -> ident:string -> t
  val pointer : ?loc:Srcloc.t -> arg:t -> t
  val array : ?loc:Srcloc.t -> arg:t -> t
  val struct_ : ?loc:Srcloc.t -> fields:(string * t) list -> t
end

module Expr : sig
  module Bop : sig
    type t =
      | Add
    [@@deriving sexp_of, variants]
  end

  type t = private
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
    | Deref of {
        loc: Srcloc.t option [@sexp.option];
        arg: t;
      }
    | Addr_of of {
        loc: Srcloc.t option [@sexp.option];
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

  val int : ?loc:Srcloc.t -> value:int64 -> t
  val bool : ?loc:Srcloc.t -> value:bool -> t
  val float : ?loc:Srcloc.t -> value:float -> t
  val name : ?loc:Srcloc.t -> ident:string -> t
  val array : ?loc:Srcloc.t -> elts:t array -> t
  val subscript : ?loc:Srcloc.t -> arg:t -> idx:t -> t
  val cast : ?loc:Srcloc.t -> typ:Type_expr.t -> arg:t -> t
  val deref : ?loc:Srcloc.t -> arg:t -> t
  val addr_of : ?loc:Srcloc.t -> arg:t -> t
  val binop : ?loc:Srcloc.t -> op:Bop.t -> lhs:t -> rhs:t -> t
  val call : ?loc:Srcloc.t -> callee:t -> args:t list -> t
  val let_in : ?loc:Srcloc.t -> ident:string -> typ:Type_expr.t option -> binding:t -> body:t -> t

  val add : ?loc:Srcloc.t -> lhs:t -> rhs:t -> t

  val loc : t -> Srcloc.t option
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
    | Return of {
        loc: Srcloc.t option [@sexp.option];
        arg: Expr.t option [@sexp.option];
      }
  [@@deriving sexp_of]

  val expr : Expr.t -> t
  val block : t list -> t
  val assign : ?loc:Srcloc.t -> dst:Expr.t -> src:Expr.t -> t
  val let_ : ?loc:Srcloc.t -> ident:string -> typ:Type_expr.t option -> binding:Expr.t -> t
  val var : ?loc:Srcloc.t -> ident:string -> typ:Type_expr.t option -> binding:Expr.t -> t
  val if_ : ?loc:Srcloc.t -> cond:Expr.t -> iftrue:t -> iffalse:t option -> t
  val return : ?loc:Srcloc.t -> arg:Expr.t option -> t

  val to_block : t -> t
end

module Decl : sig
  type t = private
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
        pure: bool;
        extern_abi: string;
        extern_ident: string;
      }
  [@@deriving sexp_of]

  val type_ : ?loc:Srcloc.t -> ident:string -> binding:Type_expr.t -> t
  val let_ : ?loc:Srcloc.t -> ident:string -> typ:Type_expr.t -> binding:Expr.t -> t
  val fun_ : ?loc:Srcloc.t -> ident:string -> params:(string * Type_expr.t) list
    -> ret_type:Type_expr.t option -> body:Stmt.t -> pure:bool -> t
  val fun_expr : ?loc:Srcloc.t -> ident:string -> params:(string * Type_expr.t) list
    -> ret_type:Type_expr.t -> body:Expr.t -> t
  val fun_extern : ?loc:Srcloc.t -> ident:string -> params:(string * Type_expr.t) list
    -> ret_type:Type_expr.t option -> pure:bool -> extern_abi:string -> extern_ident:string -> t

  val loc : t -> Srcloc.t option
end

type t = Decl.t list
