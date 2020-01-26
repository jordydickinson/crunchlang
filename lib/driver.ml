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

let parse_prog_string s =
  let lexbuf = Lexing.from_string s in
  let checkpt = Parser.Incremental.prog lexbuf.lex_curr_p in
  let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  Parser.MenhirInterpreter.loop supplier checkpt

let parse_channel in_channel =
  let lexbuf = Lexing.from_channel in_channel in
  let checkpt = Parser.Incremental.prog lexbuf.lex_curr_p in
  let supplier = Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  Parser.MenhirInterpreter.loop supplier checkpt

let parse_file filename =
  In_channel.with_file filename ~f:parse_channel
