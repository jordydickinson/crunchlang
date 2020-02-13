let print_parse_expr s =
  Driver.parse_expr_string s
  |> Ast.Expr.sexp_of_t
  |> print_s

let print_parse_stmt s =
  Driver.parse_stmt_string s
  |> Ast.Stmt.sexp_of_t
  |> print_s

let print_parse_decl s =
  Driver.parse_decl_string s
  |> Ast.Decl.sexp_of_t
  |> print_s

let print_parse_decl_failure s =
  try print_parse_decl s; failwith "No exception raised"
  with e -> print_string @@ Exn.to_string e

(*** Expressions ***)

let%expect_test _ =
  print_parse_expr "1";
  [%expect {| (Int (loc (:1:0 :1:1)) (value 1)) |}]

let%expect_test _ =
  print_parse_expr "true";
  [%expect {| (Bool (loc (:1:0 :1:4)) (value true)) |}]

let%expect_test _ =
  print_parse_expr "false";
  [%expect {| (Bool (loc (:1:0 :1:5)) (value false)) |}]

let%expect_test _ =
  print_parse_expr "1.1";
  [%expect {| (Float (loc (:1:0 :1:3)) (value 1.1)) |}]

let%expect_test _ =
  print_parse_expr "1.";
  [%expect {| (Float (loc (:1:0 :1:2)) (value 1)) |}]

let%expect_test _ =
  print_parse_expr "1e1";
  [%expect {| (Float (loc (:1:0 :1:3)) (value 10)) |}]

let%expect_test _ =
  print_parse_expr "1.0e1";
  [%expect {| (Float (loc (:1:0 :1:5)) (value 10)) |}]

let%expect_test _ =
  print_parse_expr "x";
  [%expect {| (Name (loc (:1:0 :1:1)) (ident x)) |}]

let%expect_test _ =
  print_parse_expr "_x";
  [%expect {| (Name (loc (:1:0 :1:2)) (ident _x)) |}]

let%expect_test _ =
  print_parse_expr "x0";
  [%expect {| (Name (loc (:1:0 :1:2)) (ident x0)) |}]

let%expect_test _ =
  print_parse_expr "x + 1";
  [%expect {|
    (Binop (loc (:1:0 :1:5)) (op Add) (lhs (Name (loc (:1:0 :1:1)) (ident x)))
     (rhs (Int (loc (:1:4 :1:5)) (value 1)))) |}]

let%expect_test _ =
  print_parse_expr "x + y + z";
  [%expect {|
    (Binop (loc (:1:0 :1:9)) (op Add)
     (lhs
      (Binop (loc (:1:0 :1:5)) (op Add) (lhs (Name (loc (:1:0 :1:1)) (ident x)))
       (rhs (Name (loc (:1:4 :1:5)) (ident y)))))
     (rhs (Name (loc (:1:8 :1:9)) (ident z)))) |}]

let%expect_test _ =
  print_parse_expr "(x + y) + z";
  [%expect {|
    (Binop (loc (:1:0 :1:11)) (op Add)
     (lhs
      (Binop (loc (:1:1 :1:6)) (op Add) (lhs (Name (loc (:1:1 :1:2)) (ident x)))
       (rhs (Name (loc (:1:5 :1:6)) (ident y)))))
     (rhs (Name (loc (:1:10 :1:11)) (ident z)))) |}]

let%expect_test _ =
  print_parse_expr "x + (y + z)";
  [%expect {|
    (Binop (loc (:1:0 :1:11)) (op Add) (lhs (Name (loc (:1:0 :1:1)) (ident x)))
     (rhs
      (Binop (loc (:1:5 :1:10)) (op Add) (lhs (Name (loc (:1:5 :1:6)) (ident y)))
       (rhs (Name (loc (:1:9 :1:10)) (ident z)))))) |}]

let%expect_test _ =
  print_parse_expr "let xs = {} in xs";
  [%expect {|
    (Let_in (loc (:1:0 :1:17)) (ident xs)
     (binding (Array (loc (:1:9 :1:11)) (elts ())))
     (body (Name (loc (:1:15 :1:17)) (ident xs)))) |}]

let%expect_test _ =
  print_parse_expr "let xs = {1, 2, 3} in xs";
  [%expect {|
    (Let_in (loc (:1:0 :1:24)) (ident xs)
     (binding
      (Array (loc (:1:9 :1:18))
       (elts
        ((Int (loc (:1:10 :1:11)) (value 1)) (Int (loc (:1:13 :1:14)) (value 2))
         (Int (loc (:1:16 :1:17)) (value 3))))))
     (body (Name (loc (:1:22 :1:24)) (ident xs)))) |}]

let%expect_test _ =
  print_parse_expr "*x";
  [%expect {| (Deref (loc (:1:0 :1:2)) (arg (Name (loc (:1:1 :1:2)) (ident x)))) |}]

let%expect_test _ =
  print_parse_expr "*f(x)";
  [%expect {|
    (Deref (loc (:1:0 :1:5))
     (arg
      (Call (loc (:1:1 :1:5)) (callee (Name (loc (:1:1 :1:2)) (ident f)))
       (args ((Name (loc (:1:3 :1:4)) (ident x))))))) |}]

let%expect_test _ =
  print_parse_expr "f(x) + *x";
  [%expect {|
    (Binop (loc (:1:0 :1:9)) (op Add)
     (lhs
      (Call (loc (:1:0 :1:4)) (callee (Name (loc (:1:0 :1:1)) (ident f)))
       (args ((Name (loc (:1:2 :1:3)) (ident x))))))
     (rhs (Deref (loc (:1:7 :1:9)) (arg (Name (loc (:1:8 :1:9)) (ident x)))))) |}]

let%expect_test _ =
  print_parse_expr "*f(x) + x";
  [%expect {|
    (Binop (loc (:1:0 :1:9)) (op Add)
     (lhs
      (Deref (loc (:1:0 :1:5))
       (arg
        (Call (loc (:1:1 :1:5)) (callee (Name (loc (:1:1 :1:2)) (ident f)))
         (args ((Name (loc (:1:3 :1:4)) (ident x))))))))
     (rhs (Name (loc (:1:8 :1:9)) (ident x)))) |}]

let%expect_test _ =
  print_parse_expr "&x";
  [%expect {| (Addr_of (loc (:1:0 :1:2)) (arg (Name (loc (:1:1 :1:2)) (ident x)))) |}]

(*** Statements ***)

let%expect_test _ =
  print_parse_stmt "return x;";
  [%expect {| (Return (loc (:1:0 :1:9)) (arg (Name (loc (:1:7 :1:8)) (ident x)))) |}]

let%expect_test _ =
  print_parse_stmt "x := 1;";
  [%expect {|
    (Assign (loc (:1:0 :1:7)) (dst (Name (loc (:1:0 :1:1)) (ident x)))
     (src (Int (loc (:1:5 :1:6)) (value 1)))) |}]

let%expect_test _ =
  print_parse_stmt "let x = 1;";
  [%expect {|
    (Let (loc (:1:0 :1:10)) (ident x)
     (binding (Int (loc (:1:8 :1:9)) (value 1)))) |}]

let%expect_test _ =
  print_parse_stmt "let x: int64 = 1;";
  [%expect {|
    (Let (loc (:1:0 :1:17)) (ident x)
     (typ (Name (loc (:1:7 :1:12)) (ident int64)))
     (binding (Int (loc (:1:15 :1:16)) (value 1)))) |}]

let%expect_test _ =
  print_parse_stmt "var x = 1;";
  [%expect {|
    (Var (loc (:1:0 :1:10)) (ident x)
     (binding (Int (loc (:1:8 :1:9)) (value 1)))) |}]

let%expect_test _ =
  print_parse_stmt "var x: int64 = 1;";
  [%expect {|
    (Var (loc (:1:0 :1:17)) (ident x)
     (typ (Name (loc (:1:7 :1:12)) (ident int64)))
     (binding (Int (loc (:1:15 :1:16)) (value 1)))) |}]

let%expect_test _ =
  print_parse_stmt "if true {}";
  [%expect {|
    (If (loc (:1:0 :1:10)) (cond (Bool (loc (:1:3 :1:7)) (value true)))
     (iftrue (Block ()))) |}]

let%expect_test _ =
  print_parse_stmt "if true {} else {}";
  [%expect {|
    (If (loc (:1:0 :1:18)) (cond (Bool (loc (:1:3 :1:7)) (value true)))
     (iftrue (Block ())) (iffalse (Block ()))) |}]

(*** Declarations ***)

let%expect_test _ =
  print_parse_decl "fun nop!(): void {}";
  [%expect {|
    (Fun (loc (:1:0 :1:19)) (ident nop!) (params ())
     (ret_type (Name (loc (:1:12 :1:16)) (ident void))) (body (Block ()))) |}]

let%expect_test _ =
  print_parse_decl "fun nop(): void {}";
  [%expect {|
    (Fun (loc (:1:0 :1:18)) (ident nop) (params ())
     (ret_type (Name (loc (:1:11 :1:15)) (ident void))) (body (Block ()))
     (pure)) |}]

let%expect_test _ =
  print_parse_decl "fun add(x: int64, y: int64): int64 = x + y;";
  [%expect {|
    (Fun_expr (loc (:1:0 :1:43)) (ident add)
     (params
      ((x (Name (loc (:1:11 :1:16)) (ident int64)))
       (y (Name (loc (:1:21 :1:26)) (ident int64)))))
     (ret_type (Name (loc (:1:29 :1:34)) (ident int64)))
     (body
      (Binop (loc (:1:37 :1:42)) (op Add)
       (lhs (Name (loc (:1:37 :1:38)) (ident x)))
       (rhs (Name (loc (:1:41 :1:42)) (ident y)))))) |}]

let%expect_test _ =
  print_parse_decl {|
    extern("C")
    fun puts!(str: uint8*): int64 = "puts";
  |};
  [%expect {|
    (Fun_extern (loc (:1:5 :1:60)) (ident puts!)
     (params
      ((str
        (Pointer (loc (:1:36 :1:42))
         (arg (Name (loc (:1:36 :1:41)) (ident uint8)))))))
     (ret_type ((Name (loc (:1:45 :1:50)) (ident int64)))) (extern_abi C)
     (extern_ident puts)) |}]

let%expect_test _ =
  print_parse_decl_failure {|
    fun main!() {
      var xs: int64[] = {0, 1, 2};
      xs := {3, 4, 5};
    }
  |};
  [%expect {| (Crunch.Parser.MenhirBasics.Error) |}]

(*** Type Declarations ***)

let%expect_test _ =
  print_parse_decl "type t = int64;";
  [%expect {|
    (Type (loc (:1:0 :1:15)) (ident t)
     (binding (Name (loc (:1:9 :1:14)) (ident int64)))) |}]

let%expect_test _ =
  print_parse_decl {|
    type t = {
      x: int64;
      y: int64;
    };
  |};
  [%expect {|
    (Type (loc (:1:5 :1:54)) (ident t)
     (binding
      (Struct (loc (:1:14 :1:53))
       (fields
        ((x (Name (loc (:1:25 :1:30)) (ident int64)))
         (y (Name (loc (:1:41 :1:46)) (ident int64)))))))) |}]
