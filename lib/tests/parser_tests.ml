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
