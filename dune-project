(lang dune 2.6)
(name crunch)
(license "MIT")
(authors "Jordy Dickinson <jordy.dickinson@icloud.com>")
(maintainers "Jordy Dickinson <jordy.dickinson@icloud.com>")
(source (github jordydickinson/crunch))
(generate_opam_files true)
(using menhir 2.0)

(package
 (name crunch)
 (synopsis "Crunch programming language")
 (depends
  ("core" (>= 0.16.0))
  ("core_unix" (>= 0.16.0))
  ("menhir" (>= 20230608))
  ("llvm" (>= 16.0.6))))
