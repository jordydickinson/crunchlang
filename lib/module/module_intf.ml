module type S = sig
  type t

  (** [with_new_module name ~f] creates a new module with name [name] and calls
      [f] with it. After [f] returns, the module is disposed of and [f]'s return
      value is returned. The module passed to [f] should never be referenced
      outside the body of [f], as this will likely lead to a segmentation fault. *)
  val with_new_module : string -> f:(t -> 'a) -> 'a

  (** [codegen m ast] generates LLVM IR for [ast] within m. *)
  val codegen : t -> Ast.t -> unit

  (** [llir_string m] is the LLVM IR in [m] as a string. *)
  val llir_string : t -> string

  (** [write_llir m filename] writes the LLVM IR within [m] to the file with
      name [filename]. *)
  val write_llir : t -> string -> unit

  (** [dump_llir m] dumps the LLVM IR within [m] to standard error. *)
  val dump_llir : t -> unit

  (** [emit_obj m ~filename] emits object code from [m] to the file named
      [filename]. *)
  val emit_obj : t -> filename:string -> unit
end
