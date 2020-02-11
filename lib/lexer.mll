{
  open Parser

  exception Unclosed_comment
  [@@deriving sexp]
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
  | "//" [^ '\n' '\r']* { read lexbuf }
  | "(*" { block_comment 1 lexbuf }

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
  | "[" { LBRACKET }
  | "]" { RBRACKET }

  | ";" { SEMI }
  | "," { COMMA }
  | ":" { COLON }
  | ":=" { COLON_EQ }
  | "=" { EQ }

  | "+" { PLUS }
  | "*" { STAR }
  | "&" { AMP }

  | float as f { FLOAT (Float.of_string f) }
  | int as i { INT (Int64.of_string i) }
  | bang_ident as id { BANG_IDENT id }
  | ident as id { IDENT id }

  | eof { EOF }

and block_comment level =
  parse
  | "*)" { if level = 0 then read lexbuf else block_comment (level - 1) lexbuf }
  | "(*" { block_comment (level + 1) lexbuf }
  | _ { block_comment level lexbuf }
  | eof { raise Unclosed_comment }
