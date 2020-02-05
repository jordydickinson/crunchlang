let print_parse_semantic s =
  let ast = Driver.parse_prog_string s in
  let semantic, _ = Semantic.build_ast ast Semantic.Env.prelude in
  print_s @@ Semantic.sexp_of_t semantic

let%expect_test _ =
  print_parse_semantic {|
    type t = int64;
  |};
  [%expect {| ((Type (loc (:1:5 :1:20)) (ident t) (binding Int64))) |}]

let%expect_test _ =
  print_parse_semantic {|
    type t = int64;

    fun add(x: t, y: t): t = x + y;
  |};
  [%expect {|
    ((Type (loc (:1:5 :1:20)) (ident t) (binding Int64))
     (Fun_expr (loc (:1:26 :1:57)) (ident add) (params (x y))
      (typ (Fun (params (Int64 Int64)) (ret Int64)))
      (body
       (Binop (loc (:1:51 :1:56)) (op Add)
        (lhs (Name (loc (:1:51 :1:52)) (ident x) (typ Int64) (pure true)))
        (rhs (Name (loc (:1:55 :1:56)) (ident y) (typ Int64) (pure true))))))) |}]
