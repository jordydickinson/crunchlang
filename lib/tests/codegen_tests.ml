let print_ir s =
  let ast = Driver.parse_prog_string s in
  let context = Llvm.create_context () in
  let module_ = Llvm.create_module context "test" in
  Codegen.codegen ast ~module_;
  print_string @@ Llvm.string_of_llmodule module_;
  Llvm.dispose_module module_;
  Llvm.dispose_context context

let%expect_test _ =
  print_ir "fun main(): void {}";
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define void @main() {
    entry:
    } |}]

let%expect_test _ =
  print_ir "fun main(): void { return; }";
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define void @main() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main(): int64 {
      return 1 + 2;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @main() {
    entry:
      ret i64 3
    } |}]
