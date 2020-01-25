let parse_string s =
  let lexbuf = Lexing.from_string s in
  let checkpt = Parser.Incremental.prog lexbuf.lex_curr_p in
  let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  Parser.MenhirInterpreter.loop supplier checkpt
