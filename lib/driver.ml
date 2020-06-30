open Parser
module Interp = MenhirInterpreter

let parse_lexbuf (lexbuf: Lexing.lexbuf) parse =
  let open Or_error.Let_syntax in
  let checkpt = parse lexbuf.lex_curr_p in
  let supplier = Interp.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  Interp.loop_handle
    return
    (function
      | HandlingError env ->
        let fname =
          if String.is_empty lexbuf.lex_curr_p.pos_fname
          then "<unknown>"
          else lexbuf.lex_curr_p.pos_fname in
        let startp, endp = Interp.positions env in
        Or_error.error_string @@
        sprintf "%s:%i:%i:%i:%i: syntax error"
          fname
          startp.pos_lnum (startp.pos_cnum - startp.pos_bol)
          endp.pos_lnum (endp.pos_cnum - endp.pos_bol)
      | _ -> assert false)
    supplier checkpt

let parse_prog_string s =
  let lexbuf = Lexing.from_string s in
  ok_exn @@ parse_lexbuf lexbuf Incremental.prog

let parse_file filename =
  ok_exn @@
  In_channel.with_file filename
    ~f:(fun in_channel ->
        let lexbuf = Lexing.from_channel in_channel in
        lexbuf.lex_start_p <- { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
        lexbuf.lex_curr_p <- lexbuf.lex_start_p;
        parse_lexbuf lexbuf Incremental.prog)
