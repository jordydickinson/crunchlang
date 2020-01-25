let parse_expr_string s =
  let lexbuf = Lexing.from_string s in
  let checkpt = Parser.Incremental.expr_eof lexbuf.lex_curr_p in
  let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  Parser.MenhirInterpreter.loop supplier checkpt

let parse_stmt_string s =
  let lexbuf = Lexing.from_string s in
  let checkpt = Parser.Incremental.stmt_eof lexbuf.lex_curr_p in
  let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  Parser.MenhirInterpreter.loop supplier checkpt

let parse_decl_string s =
  let lexbuf = Lexing.from_string s in
  let checkpt = Parser.Incremental.decl_eof lexbuf.lex_curr_p in
  let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  Parser.MenhirInterpreter.loop supplier checkpt
