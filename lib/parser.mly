%{
  [@@@ocaml.warning "-3"]
  open Ast
%}

%token EOF

%token<int64> INT
%token<float> FLOAT
%token<string> IDENT

%start<Ast.Expr.t> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | value = INT { Expr.int ~loc:$loc ~value }
  | value = FLOAT { Expr.float ~loc:$loc ~value }
  | ident = IDENT { Expr.name ~loc:$loc ~ident }
  ;

%%

[@@@ocaml.warning "+3"]
