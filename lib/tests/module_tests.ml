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

let%expect_test _ =
  print_ir {|
    fun main(): float {
      return 1.0 + 2.0;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define double @main() {
    entry:
      ret double add (double 1.000000e+00, double 2.000000e+00)
    } |}]

let%expect_test _ =
  print_ir {|
    fun main(): int64 {
      let x = 1;
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @main() {
    entry:
      ret i64 1
    } |}]

let%expect_test _ =
  print_ir {|
    fun main(): int64 {
      let x = 1;
      return x + 2;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @main() {
    entry:
      ret i64 3
    } |}]

let%expect_test _ =
  print_ir {|
    fun main(): int64 {
      let x = 1;
      let y = 2;
      return x + y;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @main() {
    entry:
      ret i64 3
    } |}]
