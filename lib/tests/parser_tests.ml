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
    (Let (loc (:1:0 :1:17)) (ident x) (typ (Int64 (loc (:1:7 :1:12))))
     (binding (Int (loc (:1:15 :1:16)) (value 1)))) |}]

let%expect_test _ =
  print_parse_stmt "var x = 1;";
  [%expect {|
    (Var (loc (:1:0 :1:10)) (ident x)
     (binding (Int (loc (:1:8 :1:9)) (value 1)))) |}]

let%expect_test _ =
  print_parse_stmt "var x: int64 = 1;";
  [%expect {|
    (Var (loc (:1:0 :1:17)) (ident x) (typ (Int64 (loc (:1:7 :1:12))))
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
  print_parse_decl "fun nop(): void {}";
  [%expect {|
    (Fun (loc (:1:0 :1:18)) (name nop) (params ())
     (ret_type (Void (loc (:1:11 :1:15)))) (body (Block ()))) |}]
