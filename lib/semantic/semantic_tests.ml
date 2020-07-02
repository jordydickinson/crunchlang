let print_parse_semantic s =
  let ast = Driver.parse_prog_string s in
  let semantic, _ = Semantic.build_ast ast Semantic.Env.prelude in
  print_s @@ Semantic.sexp_of_t semantic

let print_semantic_failure s =
  try print_parse_semantic s; failwith "No exception raised"
  with e -> print_string @@ Exn.to_string e

let%expect_test _ =
  print_parse_semantic {|
    type t = int64;
  |};
  [%expect {|
    ((Type (loc (:2:4 :2:19)) (ident t)
      (binding (Int (bitwidth 64) (signed true))))) |}]

let%expect_test _ =
  print_parse_semantic {|
    type t = int64;

    fun add(x: t, y: t): t = x + y;
  |};
  [%expect {|
    ((Type (loc (:2:4 :2:19)) (ident t)
      (binding (Int (bitwidth 64) (signed true))))
     (Fun_expr (loc (:4:4 :4:35)) (ident add) (params (x y))
      (typ
       (Fun
        (params
         ((Int (bitwidth 64) (signed true)) (Int (bitwidth 64) (signed true))))
        (ret (Int (bitwidth 64) (signed true)))))
      (body
       (Binop (loc (:4:29 :4:34)) (op Add)
        (lhs
         (Name (loc (:4:29 :4:30)) (ident x)
          (typ (Int (bitwidth 64) (signed true))) (pure true)))
        (rhs
         (Name (loc (:4:33 :4:34)) (ident y)
          (typ (Int (bitwidth 64) (signed true))) (pure true))))))) |}]

let%expect_test _ =
  print_parse_semantic {|
    fun add(x: int32, y: int32): int32 = x + y;
  |};
  [%expect {|
    ((Fun_expr (loc (:2:4 :2:47)) (ident add) (params (x y))
      (typ
       (Fun
        (params
         ((Int (bitwidth 32) (signed true)) (Int (bitwidth 32) (signed true))))
        (ret (Int (bitwidth 32) (signed true)))))
      (body
       (Binop (loc (:2:41 :2:46)) (op Add)
        (lhs
         (Name (loc (:2:41 :2:42)) (ident x)
          (typ (Int (bitwidth 32) (signed true))) (pure true)))
        (rhs
         (Name (loc (:2:45 :2:46)) (ident y)
          (typ (Int (bitwidth 32) (signed true))) (pure true))))))) |}]

let%expect_test _ =
  print_parse_semantic {|
    fun main!(): int64 {
      var x = 1;
      x := 2;
      return x;
    }
  |};
  [%expect {|
    ((Fun (loc (:2:4 :6:5)) (ident main!) (params ())
      (typ (Fun (params ()) (ret (Int (bitwidth 64) (signed true)))))
      (body
       (Block
        ((Var (loc (:3:6 :3:16)) (ident x)
          (typ (Int (bitwidth 32) (signed true)))
          (binding
           (Int (loc (:3:14 :3:15)) (value 1)
            (typ (Int (bitwidth 32) (signed true))))))
         (Assign (loc (:4:6 :4:13))
          (dst
           (Name (loc (:4:6 :4:7)) (ident x)
            (typ (Int (bitwidth 32) (signed true))) (pure false)))
          (src
           (Int (loc (:4:11 :4:12)) (value 2)
            (typ (Int (bitwidth 32) (signed true))))))
         (Return (loc (:5:6 :5:15))
          (arg
           (Cast (typ (Int (bitwidth 64) (signed true)))
            (arg
             (Name (loc (:5:13 :5:14)) (ident x)
              (typ (Int (bitwidth 32) (signed true))) (pure false)))))))))
      (pure false))) |}]

let%expect_test _ =
  print_parse_semantic {|
    fun main!(): int64 {
      let x = 1;
      return x;
    }
  |};
  [%expect {|
    ((Fun (loc (:2:4 :5:5)) (ident main!) (params ())
      (typ (Fun (params ()) (ret (Int (bitwidth 64) (signed true)))))
      (body
       (Block
        ((Let (loc (:3:6 :3:16)) (ident x)
          (typ (Int (bitwidth 32) (signed true)))
          (binding
           (Int (loc (:3:14 :3:15)) (value 1)
            (typ (Int (bitwidth 32) (signed true))))))
         (Return (loc (:4:6 :4:15))
          (arg
           (Cast (typ (Int (bitwidth 64) (signed true)))
            (arg
             (Name (loc (:4:13 :4:14)) (ident x)
              (typ (Int (bitwidth 32) (signed true))) (pure true)))))))))
      (pure false))) |}]

let%expect_test _ =
  print_parse_semantic {|
    fun foo(): int32 {
      let xs = {1, 2, 3};
      return xs[1];
    }
  |};
  [%expect {|
    ((Fun (loc (:2:4 :5:5)) (ident foo) (params ())
      (typ (Fun (params ()) (ret (Int (bitwidth 32) (signed true)))))
      (body
       (Block
        ((Let (loc (:3:6 :3:25)) (ident xs)
          (typ (Array (elt (Int (bitwidth 32) (signed true))) (size 3)))
          (binding
           (Array (loc (:3:15 :3:24))
            (elts
             ((Int (loc (:3:16 :3:17)) (value 1)
               (typ (Int (bitwidth 32) (signed true))))
              (Int (loc (:3:19 :3:20)) (value 2)
               (typ (Int (bitwidth 32) (signed true))))
              (Int (loc (:3:22 :3:23)) (value 3)
               (typ (Int (bitwidth 32) (signed true))))))
            (typ (Array (elt (Int (bitwidth 32) (signed true))) (size 3))))))
         (Return (loc (:4:6 :4:19))
          (arg
           (Subscript (loc (:4:13 :4:18))
            (arg
             (Name (loc (:4:13 :4:15)) (ident xs)
              (typ (Array (elt (Int (bitwidth 32) (signed true))) (size 3)))
              (pure true)))
            (idx
             (Int (loc (:4:16 :4:17)) (value 1)
              (typ (Int (bitwidth 32) (signed true)))))))))))
      (pure true))) |}]

let%expect_test _ =
  print_parse_semantic {|
    fun main!() {
      var x = 1;
      var y: int32& = x;
      y := 2;
    }
  |};
  [%expect {|
    ((Fun (loc (:2:4 :6:5)) (ident main!) (params ())
      (typ (Fun (params ()) (ret Void)))
      (body
       (Block
        ((Var (loc (:3:6 :3:16)) (ident x)
          (typ (Int (bitwidth 32) (signed true)))
          (binding
           (Int (loc (:3:14 :3:15)) (value 1)
            (typ (Int (bitwidth 32) (signed true))))))
         (Var (loc (:4:6 :4:24)) (ident y)
          (typ (Reference (Int (bitwidth 32) (signed true))))
          (binding
           (Addr_of
            (Name (loc (:4:22 :4:23)) (ident x)
             (typ (Int (bitwidth 32) (signed true))) (pure false)))))
         (Assign (loc (:5:6 :5:13))
          (dst
           (Deref
            (Name (loc (:5:6 :5:7)) (ident y)
             (typ (Reference (Int (bitwidth 32) (signed true)))) (pure false))))
          (src
           (Int (loc (:5:11 :5:12)) (value 2)
            (typ (Int (bitwidth 32) (signed true)))))))))
      (pure false))) |}]
