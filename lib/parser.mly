%{
  [@@@ocaml.warning "-3"]
  open Ast
%}

%token EOF

%token<int64> INT
%token<float> FLOAT
%token<string> IDENT

(* Keywords *)
%token FUN "fun"
%token RETURN "return"

(* Types *)
%token VOID "void"
%token INT64 "int64"

(* Operators *)
%token PLUS "+"

(* Misc. symbols *)
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token COMMA ","
%token SEMI ";"
%token COLON ":"
%token COLON_EQ ":="

%start<Ast.t> prog
%start<Ast.Decl.t> decl_eof
%start<Ast.Stmt.t> stmt_eof
%start<Ast.Expr.t> expr_eof

%%

prog:
  | ds = decl*; EOF { ds }
  ;

decl_eof:
  | d = decl; EOF { d }
  ;

stmt_eof:
  | s = stmt; EOF { s }
  ;

expr_eof:
  | e = expr; EOF { e }
  ;

decl:
  | "fun"; name = IDENT;
    "("; params = separated_list(",", param); ")";
    ":"; ret_type = type_expr;
    "{"; body = stmt*; "}"
    { Decl.fun_ ~loc:$loc ~name ~params ~ret_type ~body }
  ;

param:
  | name = IDENT; ":"; typ = type_expr { name, typ }
  ;

stmt:
  | dst = expr; ":="; src = expr; ";" { Stmt.assign ~loc:$loc ~src ~dst }
  | "return"; arg = expr?; ";" { Stmt.return ~loc:$loc ~arg }
  ;

expr:
  | e = atom { e }
  | lhs = expr; "+"; rhs = atom { Expr.add ~loc:$loc ~lhs ~rhs }
  ;

atom:
  | value = INT { Expr.int ~loc:$loc ~value }
  | value = FLOAT { Expr.float ~loc:$loc ~value }
  | ident = IDENT { Expr.name ~loc:$loc ~ident }
  | "("; e = expr; ")" { e }
  ;

type_expr:
  | "void" { Type_expr.void ~loc:$loc }
  | "int64" { Type_expr.int64 ~loc:$loc }
  ;

%%

[@@@ocaml.warning "+3"]
