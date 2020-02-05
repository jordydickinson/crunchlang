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
  print_ir "fun main!(): void {}";
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @"main!"() {
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
  print_ir "fun main!(): void { return; }";
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define void @"main!"() {
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

    define i64 @"main!"() {
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
    fun main!(): float {
      return 1.0 + 2.0;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define double @"main!"() {
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

    define i64 @"main!"() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      ret i64 1
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

    define i64 @"main!"() {
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

    define i64 @"main!"() {
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
    fun main!(): int64 {
      var x = 1;
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @"main!"() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i64
      store i64 1, i64* %x
      %x1 = load i64, i64* %x
      ret i64 %x1
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

    define i64 @"main!"() {
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

    define i64 @"main!"() {
    entry:
      br label %body

    body:                                             ; preds = %entry
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

    define i64 @"main!"() {
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

    define i64 @"main!"(i64 %x) {
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

    define i64 @"add1!"(i64 %x) {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %addtmp = add i64 %x, 1
      ret i64 %addtmp
    }

    define i64 @"main!"() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %calltmp = call i64 @"add1!"(i64 1)
      ret i64 %calltmp
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
      var x = 1;
      x := add1!(x);
      return x;
    }
  |};
  [%expect {|
    ; ModuleID = 'test'
    source_filename = "test"

    @llvm.global_ctors = appending global [1 x void ()*] [void ()* @init.ctors]

    define i64 @"add1!"(i64 %x) {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %addtmp = add i64 %x, 1
      ret i64 %addtmp
    }

    define i64 @"main!"() {
    entry:
      br label %body

    body:                                             ; preds = %entry
      %x = alloca i64
      store i64 1, i64* %x
      %x1 = load i64, i64* %x
      %calltmp = call i64 @"add1!"(i64 %x1)
      store i64 %calltmp, i64* %x
      %x2 = load i64, i64* %x
      ret i64 %x2
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

    define i64 @"main!"() {
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
    fun main!(): void {
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

    define void @"main!"() {
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

    define i64 @"main!"() {
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

    define i64 @add(i64 %x, i64 %y) {
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

    define i64 @add(i64 %x, i64 %y) {
    entry:
      %addtmp = add i64 %x, %y
      ret i64 %addtmp
    }

    define void @init.ctors() {
    entry:
      ret void
    } |}]
