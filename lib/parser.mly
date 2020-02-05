%{
  [@@@ocaml.warning "-3"]
  open Ast
%}

%token EOF

%token<int64> INT
%token<float> FLOAT
%token<string> IDENT
%token<string> BANG_IDENT

(* Keywords *)
%token KW_TYPE "type"
%token KW_FUN "fun"
%token KW_LET "let"
%token KW_VAR "var"
%token KW_IN "in"
%token KW_IF "if"
%token KW_ELSE "else"
%token KW_RETURN "return"

(* Booleans *)
%token KW_TRUE "true"
%token KW_FALSE "false"

(* Operators *)
%token PLUS "+"

(* Misc. symbols *)
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LANGLE "<"
%token RANGLE ">"
%token COMMA ","
%token SEMI ";"
%token COLON ":"
%token COLON_EQ ":="
%token EQ "="

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
  | "type"; ident = IDENT; "="; binding = type_expr; ";"
    { Decl.type_ ~loc:$loc ~ident ~binding }
  | "let"; ident = IDENT; typ = type_annot; "="; binding = expr; ";"
    { Decl.let_ ~loc:$loc ~ident ~typ ~binding }
  | "fun"; ident = BANG_IDENT;
    "("; params = separated_list(",", param); ")";
    ret_type = type_annot;
    body = block;
    { Decl.fun_ ~loc:$loc ~ident ~params ~ret_type ~body ~pure:false }
  | "fun"; ident = IDENT;
    "("; params = separated_list(",", param); ")";
    ret_type = type_annot;
    body = block;
    { Decl.fun_ ~loc:$loc ~ident ~params ~ret_type ~body ~pure:true }
  | "fun"; ident = IDENT;
    "("; params = separated_list(",", param); ")";
    ret_type = type_annot;
    "="; body = expr; ";"
    { Decl.fun_expr ~loc:$loc ~ident ~params ~ret_type ~body }
  ;

param:
  | name = IDENT; typ = type_annot { name, typ }
  ;

stmt:
  | e = call; ";" { Stmt.expr e }
  | dst = expr; ":="; src = expr; ";"
    { Stmt.assign ~loc:$loc ~dst ~src }
  | "let"; ident = IDENT; typ = type_annot?; "="; binding = expr; ";"
    { Stmt.let_ ~loc:$loc ~ident ~typ ~binding }
  | "var"; ident = IDENT; typ = type_annot?; "="; binding = expr; ";"
    { Stmt.var ~loc:$loc ~ident ~typ ~binding }
  | if_ = if_stmt { if_ }
  | "return"; arg = expr?; ";" { Stmt.return ~loc:$loc ~arg }
  ;

block:
  | "{"; stmts = stmt*; "}" { Stmt.block stmts }
  ;

if_stmt:
  | "if"; cond = expr; iftrue = block; iffalse = else_clause?
    { Stmt.if_ ~loc:$loc ~cond ~iftrue ~iffalse }
  ;

else_clause:
  | "else"; b = block { b }
  | "else"; if_ = if_stmt { if_ }
  ;

expr:
  | e = infix { e }
  | "let"; ident = IDENT; typ = type_annot?; "="; binding = expr; "in"; body = expr
    { Expr.let_in ~loc:$loc ~ident ~typ ~binding ~body }
  ;

infix:
  | e = atom { e }
  | e = call { e }
  | lhs = infix; "+"; rhs = atom { Expr.add ~loc:$loc ~lhs ~rhs }
  ;

call:
  | callee = atom; "("; args = separated_list(",", expr); ")"
    { Expr.call ~loc:$loc ~callee ~args }
  ;

atom:
  | value = INT { Expr.int ~loc:$loc ~value }
  | value = FLOAT { Expr.float ~loc:$loc ~value }
  | "true" { Expr.bool ~loc:$loc ~value:true }
  | "false" { Expr.bool ~loc:$loc ~value:false }
  | ident = IDENT { Expr.name ~loc:$loc ~ident }
  | ident = BANG_IDENT { Expr.name ~loc:$loc ~ident }
  | "("; e = expr; ")" { e }
  | "{"; elts = separated_array(",", expr); "}" { Expr.array ~loc:$loc ~elts }
  ;

type_annot:
  | ":" typ = type_expr { typ }
  ;

type_expr:
  | ident = IDENT { Type_expr.name ~loc:$loc ~ident }
  | ident = IDENT; "<"; args = separated_array(",", type_expr); ">"
    { Type_expr.apply ~loc:$loc ~ident ~args }
  ;

separated_array(sep, term):
  | terms = separated_list(sep, term) { Array.of_list terms }
  ;

%%

[@@@ocaml.warning "+3"]
