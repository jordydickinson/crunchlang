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
      ret double 3.000000e+00
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

let%expect_test _ =
  print_ir {|
    fun main(): int64 {
      var x = 1;
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @main() {
    entry:
      %x = alloca i64
      store i64 1, i64* %x
      %x1 = load i64, i64* %x
      ret i64 %x1
    } |}]

let%expect_test _ =
  print_ir {|
    fun main(): int64 {
      var x = 1;
      x := 2;
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @main() {
    entry:
      %x = alloca i64
      store i64 1, i64* %x
      store i64 2, i64* %x
      %x1 = load i64, i64* %x
      ret i64 %x1
    } |}]

let%expect_test _ =
  print_ir {|
    fun main(): int64 {
      var x = 1;
      var y = 2;
      x := x + y;
      return x + y;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @main() {
    entry:
      %x = alloca i64
      store i64 1, i64* %x
      %y = alloca i64
      store i64 2, i64* %y
      %x1 = load i64, i64* %x
      %y2 = load i64, i64* %y
      %addtmp = add i64 %x1, %y2
      store i64 %addtmp, i64* %x
      %x3 = load i64, i64* %x
      %y4 = load i64, i64* %y
      %addtmp5 = add i64 %x3, %y4
      ret i64 %addtmp5
    } |}]

let%expect_test _ =
  print_ir {|
    fun main(): int64 {
      var x: int64 = 1;
      x := 2;
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @main() {
    entry:
      %x = alloca i64
      store i64 1, i64* %x
      store i64 2, i64* %x
      %x1 = load i64, i64* %x
      ret i64 %x1
    } |}]

let%expect_test _ =
  print_ir {|
    fun main(x: int64): int64 {
      return x + 1;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @main(i64 %x) {
    entry:
      %addtmp = add i64 %x, 1
      ret i64 %addtmp
    } |}]

let%expect_test _ =
  print_ir {|
    fun add1(x: int64): int64 {
      return x + 1;
    }

    fun main(): int64 {
      return add1(1);
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @add1(i64 %x) {
    entry:
      %addtmp = add i64 %x, 1
      ret i64 %addtmp
    }

    define i64 @main() {
    entry:
      %calltmp = call i64 @add1(i64 1)
      ret i64 %calltmp
    } |}]

let%expect_test _ =
  print_ir {|
    fun add1(x: int64): int64 {
      return x + 1;
    }

    fun main(): int64 {
      var x = 1;
      x := add1(x);
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    define i64 @add1(i64 %x) {
    entry:
      %addtmp = add i64 %x, 1
      ret i64 %addtmp
    }

    define i64 @main() {
    entry:
      %x = alloca i64
      store i64 1, i64* %x
      %x1 = load i64, i64* %x
      %calltmp = call i64 @add1(i64 %x1)
      store i64 %calltmp, i64* %x
      %x2 = load i64, i64* %x
      ret i64 %x2
    } |}]
