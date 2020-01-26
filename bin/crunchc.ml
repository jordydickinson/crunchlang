open Crunch

let link infile ~outfile =
  Sys.command_exn
  @@ sprintf "cc -o %s %s"
    (Sys.quote outfile)
    (Sys.quote infile)

let () =
  let argv = Sys.get_argv () in
  let infile = argv.(1) in
  let outfile = "a.out" in
  let tmp_fname, _tmp_fd = Unix.mkstemp (infile ^ ".o") in
  let ast = Driver.parse_file infile in
  Module.with_new_module infile ~f:begin fun m ->
    Module.codegen m ast;
    Module.emit_obj m ~filename:tmp_fname
  end;
  protect
    ~f:(fun () -> link tmp_fname ~outfile)
    ~finally:(fun () -> Sys.remove tmp_fname)
