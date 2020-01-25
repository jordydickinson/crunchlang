(lang dune 2.0)
(name crunchlib)
(license "MIT")
(authors "Jordy Dickinson <jordy.dickinson@icloud.com>")
(source (github jordydickinson/geode))
(generate_opam_files true)
(using menhir 2.0)

(package
 (name crunchlib)
 (synopsis "Crunch programming language")
 (depends
  ("core_kernel" (= 0.13))
  ("menhir" (>= 20190924))
  ("llvm" (and (>= 9.0) (< 10.0)))))
