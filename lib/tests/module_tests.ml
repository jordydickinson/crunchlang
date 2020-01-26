let print_ir s =
  let ast = Driver.parse_prog_string s in
  Module.with_new_module "test" ~f:begin fun m ->
    Module.codegen m ast;
    Module.llir_string m
    |> print_string
  end

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
