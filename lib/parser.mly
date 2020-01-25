%{
  [@@@ocaml.warning "-3"]
  open Ast
%}

%token EOF

%token<int64> INT
%token<float> FLOAT
%token<string> IDENT

%token LPAREN "("
%token RPAREN ")"

%token PLUS "+"

%start<Ast.Expr.t> prog

%%

prog:
  | e = expr; EOF { e }
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
