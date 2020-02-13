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
    ((Type (loc (:1:5 :1:20)) (ident t)
      (binding (Int (bitwidth 64) (signed true))))) |}]

let%expect_test _ =
  print_parse_semantic {|
    type t = int64;

    fun add(x: t, y: t): t = x + y;
  |};
  [%expect {|
    ((Type (loc (:1:5 :1:20)) (ident t)
      (binding (Int (bitwidth 64) (signed true))))
     (Fun_expr (loc (:1:26 :1:57)) (ident add) (params (x y))
      (typ
       (Fun
        (params
         ((Int (bitwidth 64) (signed true)) (Int (bitwidth 64) (signed true))))
        (ret (Int (bitwidth 64) (signed true)))))
      (body
       (Binop (loc (:1:51 :1:56)) (op Add)
        (lhs
         (Name (loc (:1:51 :1:52)) (ident x)
          (typ (Int (bitwidth 64) (signed true))) (pure true)))
        (rhs
         (Name (loc (:1:55 :1:56)) (ident y)
          (typ (Int (bitwidth 64) (signed true))) (pure true))))))) |}]

let%expect_test _ =
  print_parse_semantic {|
    fun add(x: int32, y: int32): int32 = x + y;
  |};
  [%expect {|
    ((Fun_expr (loc (:1:5 :1:48)) (ident add) (params (x y))
      (typ
       (Fun
        (params
         ((Int (bitwidth 32) (signed true)) (Int (bitwidth 32) (signed true))))
        (ret (Int (bitwidth 32) (signed true)))))
      (body
       (Binop (loc (:1:42 :1:47)) (op Add)
        (lhs
         (Name (loc (:1:42 :1:43)) (ident x)
          (typ (Int (bitwidth 32) (signed true))) (pure true)))
        (rhs
         (Name (loc (:1:46 :1:47)) (ident y)
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
    ((Fun (loc (:1:5 :1:78)) (ident main!) (params ())
      (typ (Fun (params ()) (ret (Int (bitwidth 64) (signed true)))))
      (body
       (Block
        ((Var (loc (:1:32 :1:42)) (ident x)
          (typ (Int (bitwidth 32) (signed true)))
          (binding
           (Int (loc (:1:40 :1:41)) (value 1)
            (typ (Int (bitwidth 32) (signed true))))))
         (Assign (loc (:1:49 :1:56))
          (dst
           (Name (loc (:1:49 :1:50)) (ident x)
            (typ (Int (bitwidth 32) (signed true))) (pure false)))
          (src
           (Int (loc (:1:54 :1:55)) (value 2)
            (typ (Int (bitwidth 32) (signed true))))))
         (Return (loc (:1:63 :1:72))
          (arg
           (Coerce (typ (Int (bitwidth 64) (signed true)))
            (arg
             (Name (loc (:1:70 :1:71)) (ident x)
              (typ (Int (bitwidth 32) (signed true))) (pure false)))))))))
      (pure false))) |}]

let%expect_test _ =
  print_semantic_failure {|
    fun main!() {
      var x: int32 = 1;
      var y: int64* = &x;
      *y := 2;
    }
  |};
  [%expect {|
    (lib/semantic/semantic.ml.Coercion_error (loc (:1:65 :1:67))
      (dst_type (Pointer (Int (bitwidth 64) (signed true))))
      (src_type (Pointer (Int (bitwidth 32) (signed true))))) |}]

let%expect_test _ =
  print_parse_semantic {|
    fun main!(): int64 {
      let x = 1;
      return x;
    }
  |};
  [%expect {|
    ((Fun (loc (:1:5 :1:64)) (ident main!) (params ())
      (typ (Fun (params ()) (ret (Int (bitwidth 64) (signed true)))))
      (body
       (Block
        ((Let (loc (:1:32 :1:42)) (ident x)
          (typ (Int (bitwidth 32) (signed true)))
          (binding
           (Int (loc (:1:40 :1:41)) (value 1)
            (typ (Int (bitwidth 32) (signed true))))))
         (Return (loc (:1:49 :1:58))
          (arg
           (Coerce (typ (Int (bitwidth 64) (signed true)))
            (arg
             (Name (loc (:1:56 :1:57)) (ident x)
              (typ (Int (bitwidth 32) (signed true))) (pure true)))))))))
      (pure false))) |}]
