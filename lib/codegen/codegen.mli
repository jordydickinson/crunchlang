open LLVM

val emit_obj : llmodule -> filename:string -> unit

val codegen_ast : llmodule -> Ast.t -> unit
