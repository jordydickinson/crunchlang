(lang dune 2.6)
(name crunch)
(license "MIT")
(authors "Jordy Dickinson <jordy.dickinson@icloud.com>")
(source (github jordydickinson/crunch))
(generate_opam_files true)
(using menhir 2.0)

(package
 (name crunch)
 (synopsis "Crunch programming language")
 (depends
  ("core" (= 0.13))
  ("menhir" (>= 20200123))
  ("llvm" (and (>= 10.0) (< 11.0)))))
