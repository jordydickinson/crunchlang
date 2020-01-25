%{
  [@@@ocaml.warning "-3"]
  open Ast
%}

%token EOF

%token<int64> INT
%token<float> FLOAT
%token<string> IDENT

(* Keywords *)
%token RETURN "return"

(* Operators *)
%token PLUS "+"

(* Misc. symbols *)
%token LPAREN "("
%token RPAREN ")"
%token SEMI ";"
%token COLON_EQ ":="

%start<Ast.Stmt.t> stmt_eof
%start<Ast.Expr.t> expr_eof

%%

stmt_eof:
  | s = stmt; EOF { s }
  ;

expr_eof:
  | e = expr; EOF { e }
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

%%

[@@@ocaml.warning "+3"]
