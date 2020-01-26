module type S = sig
  val codegen : Ast.t -> module_:Llvm.llmodule -> unit
end
