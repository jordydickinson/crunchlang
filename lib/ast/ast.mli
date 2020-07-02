(** The Crunch AST. This file contains a version of the AST that's relatively
    close to the user input. *)

module Type_expr : sig
  (** Type expression nodes correspond to user input appearing in "type
      positions", such as type annotations. They are converted to concrete types
      in later phases. *)

  (** [Type_expr.t] is a type expression. *)
  type t = private
    | Name of { loc: Srcloc.t option [@sexp.option]; ident: string }
    | Reference of { loc: Srcloc.t option [@sexp.option]; arg: t }
    | Array of { loc: Srcloc.t option [@sexp.option]; arg: t }
    | Struct of { loc: Srcloc.t option [@sexp.option]; fields: (string * t) list }
  [@@deriving sexp_of]

  (** [name ?loc ~ident] is a typename type expr node with source location [loc]
      and name [ident]. *)
  val name : ?loc:Srcloc.t -> ident:string -> t

  (** [reference ?loc ~arg] is a reference type expr node with source location
      [loc] and representing the type [arg&]. *)
  val reference : ?loc:Srcloc.t -> arg:t -> t

  (** [array ?loc ~arg] is an array type expr node with source location [loc]
      and representing the type [arg[]]. *)
  val array : ?loc:Srcloc.t -> arg:t -> t

  (** [struct_ ?loc ~fields:[(fieldname, fieldtype); ...]] is a structure type
      node with source location [loc] and representing the type
      [{ fieldname: fieldtype; ... }].  *)
  val struct_ : ?loc:Srcloc.t -> fields:(string * t) list -> t
end

module Expr : sig
  (** Expression nodes correspond to user input appearing in "expression
      positions"; i.e., not types, statements, or declarations. *)

  module Bop : sig
    (** A simple module for the type of binary operators. *)

    (** [Expr.Bop.t] corresponds to an operator, for use with the [Binop]
        variant of the [Ast.Expr.t] type. *)
    type t =
      | Add (** [Add] represents the [+] operator. *)
      | Lt (** [Lt] represents the [<] operator. *)
    [@@deriving sexp_of]
  end

  (** [Expr.t] is an expression node. *)
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

  (** [int ?loc ~value] is an expression node with source location [loc]
      representing the integer [value]. *)
  val int : ?loc:Srcloc.t -> value:int64 -> t

  (** [bool ?loc ~value] is an expression node with source location [loc]
      representing the boolean [value]. *)
  val bool : ?loc:Srcloc.t -> value:bool -> t

  (** [float ?loc ~value] is an expression node with source location [loc]
      representing the floating point [value]. *)
  val float : ?loc:Srcloc.t -> value:float -> t

  (** [string ?loc ~value] is an expression node with source location [loc]
      representing the string [value]. *)
  val string : ?loc:Srcloc.t -> value:string -> t

  (** [name ?loc ~ident] is an expression node with source location [loc]
      representing an identifier named [ident]. *)
  val name : ?loc:Srcloc.t -> ident:string -> t

  (** [array ?loc ~elts] is an expression node with source location [loc]
      representing an array with elements [elts]. *)
  val array : ?loc:Srcloc.t -> elts:t array -> t

  (** [subscript ?loc ~arg ~idx] is an expression node with source location
      [loc] representing the array subscripting operation [arg[idx]]. *)
  val subscript : ?loc:Srcloc.t -> arg:t -> idx:t -> t

  (** [cast ?loc ~typ ~arg] is an expression node with source location [loc]
      representing the cast operation [arg as typ]. *)
  val cast : ?loc:Srcloc.t -> typ:Type_expr.t -> arg:t -> t

  (** [binop ?loc ~op ~lhs ~rhs] is an expression node with source location
      [loc] representing the binary operation [lhs op rhs]. *)
  val binop : ?loc:Srcloc.t -> op:Bop.t -> lhs:t -> rhs:t -> t

  (** [call ?loc ~callee ~args:[arg1; ...]] is an expression node with source
      location [loc] representing the function call [callee(arg1, ...)]. *)
  val call : ?loc:Srcloc.t -> callee:t -> args:t list -> t

  (** [let_in ?loc ~ident ~typ ~binding ~body] is an expression node with source
      location [loc] representing the binding expression
      [let ident : typ = binding in body]. *)
  val let_in : ?loc:Srcloc.t -> ident:string -> typ:Type_expr.t option -> binding:t -> body:t -> t

  (** [add ?loc ~lhs ~rhs] is the same as [binop ?loc ~op:Bop.Add ~lhs ~rhs]. *)
  val add : ?loc:Srcloc.t -> lhs:t -> rhs:t -> t

  (** [lt ?loc ~lhs ~rhs] is the same as [binop ?loc ~op:Bop.Lt ~lhs ~rhs]. *)
  val lt : ?loc:Srcloc.t -> lhs:t -> rhs:t -> t

  (** [loc expr] is the source location of [expr], if any. *)
  val loc : t -> Srcloc.t option
end

module Stmt : sig
  (** Statement nodes are AST nodes found in "statement positions"; i.e., inside
      the block of a function body, or within a control flow statement. *)
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

  (** [expr e] is a statement node containing only the expression node [e], and
      represents the statement [e;]. *)
  val expr : Expr.t -> t

  (** [block [stmt; ...]] is a statement node representing the block statement
      [{ stmt; ... }]. *)
  val block : t list -> t

  (** [assign ?loc ~dst ~src] is a statement node with source location [loc]
      representing the assignment operation [dst := src;]. *)
  val assign : ?loc:Srcloc.t -> dst:Expr.t -> src:Expr.t -> t

  (** [let_ ?loc ~ident ~typ ~binding] is a let-binding statement with source
      location [loc], representing the statement [let ident : typ = binding;]. *)
  val let_ : ?loc:Srcloc.t -> ident:string -> typ:Type_expr.t option -> binding:Expr.t -> t

  (** [var ?loc ~ident ~typ ~binding] is a var-binding statement with source
      location [loc], representing the statement [var ident : typ = binding;]. *)
  val var : ?loc:Srcloc.t -> ident:string -> typ:Type_expr.t option -> binding:Expr.t -> t

  (** [if_ ?loc ~cond ~iftrue ~iffalse] is an if statement with source location
      [loc], representing the control flow structure
      [if cond { iftrue } else { iffalse }]. *)
  val if_ : ?loc:Srcloc.t -> cond:Expr.t -> iftrue:t -> iffalse:t option -> t

  (** [while_ ?loc ~cond ~body] is a statement node with source location [loc]
      representing the while loop [while cond { body }]. *)
  val while_ : ?loc:Srcloc.t -> cond:Expr.t -> body:t -> t

  (** [return ?loc ~arg] is a statement with source location [loc], representing
      the return statement [return arg;]. *)
  val return : ?loc:Srcloc.t -> arg:Expr.t option -> t

  (** [to_block stmt] converts [stmt] to a block statement, if it isn't one
      already. For example, the statement [dst := src;] would be converted to
      [{ dst := src; }], whereas [{ stmt; ... }] would be unchanged. *)
  val to_block : t -> t
end

module Decl : sig
  (** Declaration nodes correspond to top-level declarations, such as function
      and global variable declarations and definitions. *)

  (** [Decl.t] is a declaration node. *)
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

  (** [type_ ?loc ~ident ~binding] is a declaration node with source location
      [loc] corresponding to the type declaration [type ident = binding;]. *)
  val type_ : ?loc:Srcloc.t -> ident:string -> binding:Type_expr.t -> t

  (** [let_ ?loc ~ident ~typ ~binding] is a global let binding with source
      location [loc], corresponding to the declaration
      [let ident : typ = binding;]. *)
  val let_ : ?loc:Srcloc.t -> ident:string -> typ:Type_expr.t -> binding:Expr.t -> t

  (** [fun_ ?loc ~ident ~params:[(param_ident, param_type); ...] ~ret_type ~body ~pure]
      is a declaration node with source location [loc] corresponding to the
      function declaration

      {[
        fun ident(param_ident: param_type, ...): ret_type {
          body
        }
      ]}

      The [pure] parameter controls whether this function is to be considered
      "pure" during semantic analysis. A function declared as pure will have its
      body checked for purity, and will be considered a pure function for the
      purposes for checking other pure nodes.

      Note that while the parser requires that impure functions end with [!],
      this constructor does not enforce it. Note however that Crunch assumes
      this convention to apply during code generation, so if interoperability
      with Crunch is desired, you should as well. *)
  (* TODO: A better approach to the naming of identifiers and their purity may
     be desired. *)
  val fun_ : ?loc:Srcloc.t -> ident:string -> params:(string * Type_expr.t) list
    -> ret_type:Type_expr.t option -> body:Stmt.t -> pure:bool -> t

  (** [fun_expr ?loc ~ident ~params:[(param_ident, param_type); ...] ~ret_type ~body]
      is a declaration node with source location [loc], representing the
      expression function

      {[
        fun ident(param_ident: param_type, ...): ret_type = body;
      ]}

      Unlike functions created with [fun_], these are always pure, and thus there
      is no [pure] parameter. The same naming considerations regarding the use
      of [!] in identifiers applies. *)
  val fun_expr : ?loc:Srcloc.t -> ident:string -> params:(string * Type_expr.t) list
    -> ret_type:Type_expr.t -> body:Expr.t -> t

  (** [fun_extern ?loc ~ident ~params:[(param_name, param_type); ...] ~ret_type ~pure ~extern_abi ~extern_ident]
      is the declaration node with source location [loc] corresponding to the
      external function declaration

      {[
        extern("extern_abi")
          fun ident(param_name: param_type, ...): ret_type := "extern_ident";
      ]}

      The same naming conventions as [fun_] and [fun_expr] regarding the use of
      [!] in identifiers applies. However, note that only [extern_ident] matters
      when it comes to the name of the external function that this declaration
      represents. *)
  val fun_extern : ?loc:Srcloc.t -> ident:string -> params:(string * Type_expr.t) list
    -> ret_type:Type_expr.t option -> pure:bool -> extern_abi:string -> extern_ident:string -> t

  (** [loc stmt] is the source location of [stmt], if any. *)
  val loc : t -> Srcloc.t option
end

type t = Decl.t list
