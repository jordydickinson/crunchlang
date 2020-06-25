open LLVM

val global_context : unit -> llcontext

val create_context : unit -> llcontext

val create_module : llcontext -> string -> llmodule

val emit_obj : llmodule -> filename:string -> unit

val dump_module : llmodule -> unit

val string_of_llmodule : llmodule -> string

val codegen_ast : llmodule -> Ast.t -> unit
