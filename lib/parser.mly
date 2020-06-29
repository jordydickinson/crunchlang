%{
  [@@@ocaml.warning "-3"]
  open Ast
%}

%token EOF

%token<int64> INT
%token<float> FLOAT
%token<string> STRING
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
%token KW_EXTERN "extern"
%token KW_AS "as"
%token KW_WHILE "while"

(* Booleans *)
%token KW_TRUE "true"
%token KW_FALSE "false"

(* Operators *)
%token PLUS "+"
%token STAR "*"
%token AMP "&"
%token LT "<"

(* Misc. symbols *)
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACKET "["
%token RBRACKET "]"
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
  | "let" ident = IDENT typ = type_annot "=" binding = init_expr ";"
    { Decl.let_ ~loc:$loc ~ident ~typ ~binding }
  | "fun"; ident = BANG_IDENT;
    "("; params = separated_list(",", param); ")";
    ret_type = type_annot?;
    body = block;
    { Decl.fun_ ~loc:$loc ~ident ~params ~ret_type ~body ~pure:false }
  | "fun"; ident = IDENT;
    "("; params = separated_list(",", param); ")";
    ret_type = type_annot;
    body = block;
    { Decl.fun_ ~loc:$loc ~ident ~params ~ret_type:(Some ret_type) ~body ~pure:true }
  | "fun"; ident = IDENT;
    "("; params = separated_list(",", param); ")";
    ret_type = type_annot;
    "="; body = expr; ";"
    { Decl.fun_expr ~loc:$loc ~ident ~params ~ret_type ~body }
  | "extern" "(" extern_abi = STRING ")"
    "fun" ident = IDENT
    "(" params = separated_list(",", param) ")"
    ret_type = type_annot?
    "=" extern_ident = STRING ";"
    { Decl.fun_extern ~loc:$loc ~ident ~params ~ret_type ~extern_abi ~extern_ident ~pure:true }
  | "extern" "(" extern_abi = STRING ")"
    "fun" ident = BANG_IDENT
    "(" params = separated_list(",", param) ")"
    ret_type = type_annot?
    "=" extern_ident = STRING ";"
    { Decl.fun_extern ~loc:$loc ~ident ~params ~ret_type ~extern_abi ~extern_ident ~pure:false }
  ;

param:
  | name = IDENT; typ = type_annot { name, typ }
  ;

stmt:
  | e = expr; ";" { Stmt.expr e }
  | dst = expr; ":="; src = expr; ";"
    { Stmt.assign ~loc:$loc ~dst ~src }
  | "let" ident = IDENT typ = type_annot? "=" binding = init_expr ";"
    { Stmt.let_ ~loc:$loc ~ident ~typ ~binding }
  | "var" ident = IDENT typ = type_annot? "=" binding = init_expr ";"
    { Stmt.var ~loc:$loc ~ident ~typ ~binding }
  | if_ = if_stmt { if_ }
  | "while" cond = expr body = block { Stmt.while_ ~loc:$loc ~cond ~body }
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
  | e = compare { e }
  | "let" ident = IDENT typ = type_annot? "=" binding = init_expr "in" body = expr
    { Expr.let_in ~loc:$loc ~ident ~typ ~binding ~body }
  ;

init_expr:
  | e = expr { e }
  | "{" elts = separated_array(",", init_expr) "}" { Expr.array ~loc:$loc ~elts }
  ;

compare:
  | e = cast { e }
  | lhs = cast "<" rhs = cast { Expr.lt ~loc:$loc ~lhs ~rhs }

cast:
  | e = infix { e }
  | arg = infix "as" typ = type_expr { Expr.cast ~loc:$loc ~arg ~typ }
  ;

infix:
  | e = prefix { e }
  | lhs = infix; "+"; rhs = prefix { Expr.add ~loc:$loc ~lhs ~rhs }
  ;

prefix:
  | e = call { e }
  | "*"; arg = prefix { Expr.deref ~loc:$loc ~arg }
  | "&"; arg = prefix { Expr.addr_of ~loc:$loc ~arg }
  ;

call:
  | e = atom { e }
  | callee = atom; "("; args = separated_list(",", expr); ")"
    { Expr.call ~loc:$loc ~callee ~args }
  | arg = atom "[" idx = expr "]" { Expr.subscript ~loc:$loc ~arg ~idx }
  ;

atom:
  | value = INT { Expr.int ~loc:$loc ~value }
  | value = FLOAT { Expr.float ~loc:$loc ~value }
  | "true" { Expr.bool ~loc:$loc ~value:true }
  | "false" { Expr.bool ~loc:$loc ~value:false }
  | ident = IDENT { Expr.name ~loc:$loc ~ident }
  | ident = BANG_IDENT { Expr.name ~loc:$loc ~ident }
  | "("; e = expr; ")" { e }
  ;

type_annot:
  | ":" typ = type_expr { typ }
  ;

type_expr:
  | ident = IDENT { Type_expr.name ~loc:$loc ~ident }
  | arg = type_expr "*" { Type_expr.pointer ~loc:$loc ~arg }
  | arg = type_expr "[" "]" { Type_expr.array ~loc:$loc ~arg }
  | "{"; fields = separated_list(";", field); "}"
  | "{"; fields = field_semi+; "}"
    { Type_expr.struct_ ~loc:$loc ~fields }
  ;

%inline field:
  | ident = IDENT; typ = type_annot { ident, typ }
  ;

%inline field_semi:
  | field = field ";" { field }
  ;

separated_array(sep, term):
  | terms = separated_list(sep, term) { Array.of_list terms }
  ;

%%

[@@@ocaml.warning "+3"]
