{
  open Parser
}

let white = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let int = '-'? digit+
let fractional_part = '.' digit*
let exponent_part = ('e' | 'E') ('+' | '-')? digit+
let float = '-'? digit+ (fractional_part exponent_part? | exponent_part)
let letter = ['a'-'z' 'A'-'Z']
let ident = ('_' | letter) ('_' | letter | digit)*
let bang_ident = ident '!'

rule read =
  parse
  | white { read lexbuf }

  | "type" { KW_TYPE }
  | "fun" { KW_FUN }
  | "let" { KW_LET }
  | "var" { KW_VAR }
  | "in" { KW_IN }
  | "if" { KW_IF }
  | "else" { KW_ELSE }
  | "return" { KW_RETURN }

  | "true" { KW_TRUE }
  | "false" { KW_FALSE }

  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }

  | ";" { SEMI }
  | "," { COMMA }
  | ":" { COLON }
  | ":=" { COLON_EQ }
  | "=" { EQ }

  | "+" { PLUS }

  | float as f { FLOAT (Float.of_string f) }
  | int as i { INT (Int64.of_string i) }
  | bang_ident as id { BANG_IDENT id }
  | ident as id { IDENT id }

  | eof { EOF }
