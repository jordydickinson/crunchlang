let link infile ~outfile =
  Sys.command_exn
  @@ sprintf "cc -o %s %s"
    (Sys.quote outfile)
    (Sys.quote infile)

let compile ?(outfile = "a.out") infile =
  let open Crunch in
  let tmp_fname, _tmp_fd = Unix.mkstemp (infile ^ ".o") in
  protect ~f:(fun () ->
      let ast = Driver.parse_file infile in
      let ctx = Codegen.create_context () in
      let m = Codegen.create_module ctx infile in
      Codegen.codegen_ast m ast;
      Codegen.emit_obj m ~filename:tmp_fname;
      link tmp_fname ~outfile)
    ~finally:(fun () -> Sys.remove tmp_fname)

let compile_cmd =
  Command.basic
    ~summary:"Compile a Crunch input file to an executable"
    Command.Let_syntax.(
      let%map_open infile = anon ("path" %: Filename.arg_type)
      and outfile = flag "-outfile" (optional Filename.arg_type) ~doc:"path output file" in
      fun () -> compile infile ?outfile)

let () = Command.run compile_cmd
