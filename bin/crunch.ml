let link infile ~outfile =
  Sys.command_exn
  @@ sprintf "cc -o %s %s"
    (Sys.quote outfile)
    (Sys.quote infile)

let compile ?(outfile = "a.out") infile ~dump_ir =
  let open Crunch in
  let ast = Driver.parse_file infile in
  let ctx = Codegen.create_context () in
  let m = Codegen.create_module ctx infile in
  Codegen.codegen_ast m ast;
  if dump_ir then Codegen.dump_module m;
  let tmp_fname, _tmp_fd = Unix.mkstemp (infile ^ ".o") in
  Codegen.emit_obj m ~filename:tmp_fname;
  link tmp_fname ~outfile;
  Sys.remove tmp_fname

let compile_cmd =
  Command.basic
    ~summary:"Compile a Crunch input file to an executable"
    Command.Let_syntax.(
      let%map_open infile = anon ("path" %: Filename.arg_type)
      and outfile = flag "-output" (optional Filename.arg_type) ~doc:"path output file"
      and dump_ir = flag "-dump-ir" no_arg ~doc:"dump LLVM IR to stderr" in
      fun () -> compile infile ?outfile ~dump_ir)

let print_ir =
  Command.basic
    ~summary:"Compile a Crunch source file to LLVM IR"
    Command.Let_syntax.(
      let%map_open infile = anon ("path" %: Filename.arg_type)
      and outfile = flag "-output" (optional Filename.arg_type) ~doc:"path output file" in
      fun () ->
        let open Crunch in
        let module_name = Filename.basename infile |> Filename.chop_extension in
        let outfile = Option.value outfile ~default:(Filename.chop_extension infile ^ ".ll") in
        let ast = Driver.parse_file infile in
        let ctx = Codegen.create_context () in
        let m = Codegen.create_module ctx module_name in
        Codegen.codegen_ast m ast;
        let llir = Codegen.string_of_llmodule m in
        Out_channel.write_all outfile ~data:llir)

let () = Command.run @@ Command.group ~summary:"Crunch compiler driver"
    [ "compile", compile_cmd
    ; "print-ir", print_ir
    ]
