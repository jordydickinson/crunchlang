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

rule read =
  parse
  | white { read lexbuf }
  | "return" { RETURN }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";" { SEMI }
  | ":=" { COLON_EQ }
  | float as f { FLOAT (Float.of_string f) }
  | int as i { INT (Int64.of_string i) }
  | ident as id { IDENT id }
  | eof { EOF }
