let print_parse s =
  Driver.parse_string s
  |> Ast.Expr.sexp_of_t
  |> print_s

let%expect_test _ =
  print_parse "1";
  [%expect {| (Int (loc (:1:0 :1:1)) (value 1)) |}]

let%expect_test _ =
  print_parse "1.1";
  [%expect {| (Float (loc (:1:0 :1:3)) (value 1.1)) |}]

let%expect_test _ =
  print_parse "1.";
  [%expect {| (Float (loc (:1:0 :1:2)) (value 1)) |}]

let%expect_test _ =
  print_parse "1e1";
  [%expect {| (Float (loc (:1:0 :1:3)) (value 10)) |}]

let%expect_test _ =
  print_parse "1.0e1";
  [%expect {| (Float (loc (:1:0 :1:5)) (value 10)) |}]

let%expect_test _ =
  print_parse "x";
  [%expect {| (Name (loc (:1:0 :1:1)) (ident x)) |}]

let%expect_test _ =
  print_parse "_x";
  [%expect {| (Name (loc (:1:0 :1:2)) (ident _x)) |}]

let%expect_test _ =
  print_parse "x0";
  [%expect {| (Name (loc (:1:0 :1:2)) (ident x0)) |}]

let%expect_test _ =
  print_parse "x + 1";
  [%expect {|
    (Binop (loc (:1:0 :1:5)) (op Add) (lhs (Name (loc (:1:0 :1:1)) (ident x)))
     (rhs (Int (loc (:1:4 :1:5)) (value 1)))) |}]

let%expect_test _ =
  print_parse "x + y + z";
  [%expect {|
    (Binop (loc (:1:0 :1:9)) (op Add)
     (lhs
      (Binop (loc (:1:0 :1:5)) (op Add) (lhs (Name (loc (:1:0 :1:1)) (ident x)))
       (rhs (Name (loc (:1:4 :1:5)) (ident y)))))
     (rhs (Name (loc (:1:8 :1:9)) (ident z)))) |}]

let%expect_test _ =
  print_parse "(x + y) + z";
  [%expect {|
    (Binop (loc (:1:0 :1:11)) (op Add)
     (lhs
      (Binop (loc (:1:1 :1:6)) (op Add) (lhs (Name (loc (:1:1 :1:2)) (ident x)))
       (rhs (Name (loc (:1:5 :1:6)) (ident y)))))
     (rhs (Name (loc (:1:10 :1:11)) (ident z)))) |}]

let%expect_test _ =
  print_parse "x + (y + z)";
  [%expect {|
    (Binop (loc (:1:0 :1:11)) (op Add) (lhs (Name (loc (:1:0 :1:1)) (ident x)))
     (rhs
      (Binop (loc (:1:5 :1:10)) (op Add) (lhs (Name (loc (:1:5 :1:6)) (ident y)))
       (rhs (Name (loc (:1:9 :1:10)) (ident z)))))) |}]
