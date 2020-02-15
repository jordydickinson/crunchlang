let print_ir s =
  let open LLVM in
  let ast = Driver.parse_prog_string s in
  let ctx = create_context () in
  let modul = create_module ctx "test" in
  Codegen.codegen_ast modul ast;
  print_string @@ string_of_llmodule modul;
  dispose_module modul;
  dispose_context ctx

let%expect_test _ =
  print_ir "fun main!() {}";
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @main() {
    entry:
      br label %exit

    exit:                                             ; preds = %entry
      ret void
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir "fun main!() { return; }";
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      ret void
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int64 {
      return 1 + 2;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      ret i64 3
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): float64 {
      return 1.0 + 2.0;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define double @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      ret double 3.000000e+00
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int64 {
      let x = 1;
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i32
      store i32 1, i32* %x
      %x1 = load i32, i32* %x
      %x1.coerced = sext i32 %x1 to i64
      ret i64 %x1.coerced
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int64 {
      let x = 1;
      return x + 2;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i32
      store i32 1, i32* %x
      %x1 = load i32, i32* %x
      %x1.coerced = sext i32 %x1 to i64
      %addtmp = add i64 %x1.coerced, 2
      ret i64 %addtmp
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int64 {
      let x = 1;
      let y = 2;
      return x + y;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i32
      store i32 1, i32* %x
      %y = alloca i32
      store i32 2, i32* %y
      %x1 = load i32, i32* %x
      %x1.coerced = sext i32 %x1 to i64
      %y2 = load i32, i32* %y
      %y2.coerced = sext i32 %y2 to i64
      %addtmp = add i64 %x1.coerced, %y2.coerced
      ret i64 %addtmp
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int64 {
      var x = 1;
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i32
      store i32 1, i32* %x
      %x1 = load i32, i32* %x
      %x1.coerced = sext i32 %x1 to i64
      ret i64 %x1.coerced
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int64 {
      var x = 1;
      x := 2;
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i32
      store i32 1, i32* %x
      store i32 2, i32* %x
      %x1 = load i32, i32* %x
      %x1.coerced = sext i32 %x1 to i64
      ret i64 %x1.coerced
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int64 {
      var x = 1;
      var y = 2;
      x := x + y;
      return x + y;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i32
      store i32 1, i32* %x
      %y = alloca i32
      store i32 2, i32* %y
      %x1 = load i32, i32* %x
      %y2 = load i32, i32* %y
      %addtmp = add i32 %x1, %y2
      store i32 %addtmp, i32* %x
      %x3 = load i32, i32* %x
      %x3.coerced = sext i32 %x3 to i64
      %y4 = load i32, i32* %y
      %y4.coerced = sext i32 %y4 to i64
      %addtmp5 = add i64 %x3.coerced, %y4.coerced
      ret i64 %addtmp5
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int64 {
      var x: int64 = 1;
      x := 2;
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i64
      store i64 1, i64* %x
      store i64 2, i64* %x
      %x1 = load i64, i64* %x
      ret i64 %x1
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(x: int64): int64 {
      return x + 1;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main(i64 %x) {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %addtmp = add i64 %x, 1
      ret i64 %addtmp
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun add1!(x: int64): int64 {
      return x + 1;
    }

    fun main!(): int64 {
      return add1!(1);
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @add1(i64 %x) {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %addtmp = add i64 %x, 1
      ret i64 %addtmp
    }

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %calltmp = call i64 @add1(i64 1)
      ret i64 %calltmp
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun add1!(x: int32): int32 {
      return x + 1;
    }

    fun main!(): int32 {
      var x = 1;
      x := add1!(x);
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i32 @add1(i32 %x) {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %addtmp = add i32 %x, 1
      ret i32 %addtmp
    }

    define i32 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i32
      store i32 1, i32* %x
      %x1 = load i32, i32* %x
      %calltmp = call i32 @add1(i32 %x1)
      store i32 %calltmp, i32* %x
      %x2 = load i32, i32* %x
      ret i32 %x2
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int64 {
      var is_true = true;
      if is_true {
        return 1;
      } else {
        return 2;
      }
      return 3;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %is_true = alloca i1
      store i1 true, i1* %is_true
      %is_true1 = load i1, i1* %is_true
      br i1 %is_true1, label %iftrue, label %iffalse

    iftrue:                                           ; preds = %body
      ret i64 1

    iffalse:                                          ; preds = %body
      ret i64 2
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!() {
      if true {
        if false {
          return;
        } else if true {
          return;
        } else {
          return;
        }
      }
      return;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      br i1 true, label %iftrue, label %iffalse4

    iftrue:                                           ; preds = %body
      br i1 false, label %iftrue1, label %iffalse

    iftrue1:                                          ; preds = %iftrue
      ret void

    iffalse:                                          ; preds = %iftrue
      br i1 true, label %iftrue2, label %iffalse3

    iftrue2:                                          ; preds = %iffalse
      ret void

    iffalse3:                                         ; preds = %iffalse
      ret void

    iffalse4:                                         ; preds = %body
      ret void
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|let x: int64 = 1;|};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @x = global i64 undef
    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @init.ctors.x() {
    entry:
      store i64 1, i64* @x
      ret void
    }

    define void @init.ctors() {
    entry:
      call void @init.ctors.x()
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    let x: int64 = 1;
    let y: int64 = 2;
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @x = global i64 undef
    @y = global i64 undef
    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @init.ctors.x() {
    entry:
      store i64 1, i64* @x
      ret void
    }

    define void @init.ctors.y() {
    entry:
      store i64 2, i64* @y
      ret void
    }

    define void @init.ctors() {
    entry:
      call void @init.ctors.y()
      call void @init.ctors.x()
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    let x: int64 = 1;
    let y: int64 = 2;

    fun main!(): int64 {
      return x + y;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @x = global i64 undef
    @y = global i64 undef
    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @init.ctors.x() {
    entry:
      store i64 1, i64* @x
      ret void
    }

    define void @init.ctors.y() {
    entry:
      store i64 2, i64* @y
      ret void
    }

    define i64 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = load i64, i64* @x
      %y = load i64, i64* @y
      %addtmp = add i64 %x, %y
      ret i64 %addtmp
    }

    define void @init.ctors() {
    entry:
      call void @init.ctors.y()
      call void @init.ctors.x()
      ret void
    } |}]

let%expect_test _ =
  print_ir {|fun add(x: int64, y: int64): int64 = x + y;|};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @"add$pure"(i64 %x, i64 %y) {
    entry:
      %addtmp = add i64 %x, %y
      ret i64 %addtmp
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    type t = int64;

    fun add(x: t, y: t): t = x + y;
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @"add$pure"(i64 %x, i64 %y) {
    entry:
      %addtmp = add i64 %x, %y
      ret i64 %addtmp
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!() {
      var xs = {1, 2, 3};
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %xs = alloca [3 x i32]
      store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %xs
      ret void
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!() {
      var x = 1;
      var y: int32* = &x;
      *y := 2;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i32
      store i32 1, i32* %x
      %y = alloca i32*
      store i32* %x, i32** %y
      %y.deref = load i32*, i32** %y
      store i32 2, i32* %y.deref
      ret void
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    extern("C")
    fun puts!(str: uint8*): int32 = "puts";
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    declare i32 @puts(i8*)

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    extern("C")
    fun exit!(status: int32) = "exit";

    fun main!() {
      exit!(0);
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    declare void @exit(i32)

    define void @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      call void @exit(i32 0)
      ret void
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!() {
      var xs = {1, 2, 3};
      var xs_ptr: uint8* = xs as uint8*;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %xs = alloca [3 x i32]
      store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %xs
      %xs_ptr = alloca i8*
      %xs.0 = getelementptr [3 x i32], [3 x i32]* %xs, i32 0, i32 0
      %xs.0.cast = bitcast i32* %xs.0 to i8*
      store i8* %xs.0.cast, i8** %xs_ptr
      ret void
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    extern("C")
    fun puts!(str: uint8*): int32 = "puts";

    fun main!() {
        var msg: uint8[] = {
            // "Hello "
            72, 101, 108, 108, 111, 32,
            // "world\n\0"
            119, 111, 114, 108, 100, 10, 0
        };

        var _ = puts!(msg as uint8*);
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    declare i32 @puts(i8*)

    define void @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %msg = alloca [13 x i8]
      store [13 x i8] c"Hello world\0A\00", [13 x i8]* %msg
      %_ = alloca i32
      %msg.0 = getelementptr [13 x i8], [13 x i8]* %msg, i32 0, i32 0
      %calltmp = call i32 @puts(i8* %msg.0)
      store i32 %calltmp, i32* %_
      ret void
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int32 {
      let xs = {1, 2, 3};
      return xs[1];
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i32 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %xs = alloca [3 x i32]
      store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %xs
      %xs.i = getelementptr [3 x i32], [3 x i32]* %xs, i32 0, i32 1
      %xs.i.0 = load i32, i32* %xs.i
      ret i32 %xs.i.0
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]

let%expect_test _ =
  print_ir {|
    fun main!(): int32 {
      var xs = {1, 2, 3};
      xs[1] := 4;
      return xs[2];
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i32 @main() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %xs = alloca [3 x i32]
      store [3 x i32] [i32 1, i32 2, i32 3], [3 x i32]* %xs
      %xs.i = getelementptr [3 x i32], [3 x i32]* %xs, i32 0, i32 1
      store i32 4, i32* %xs.i
      %xs.i1 = getelementptr [3 x i32], [3 x i32]* %xs, i32 0, i32 2
      %xs.i1.0 = load i32, i32* %xs.i1
      ret i32 %xs.i1.0
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]
