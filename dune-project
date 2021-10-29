(lang dune 2.0)

(name koka-zero)





(license ISC)

(authors
  "Dan Gooding")

(maintainers
  "Dan Gooding")

(generate_opam_files true)

(package
 (name koka-zero)
 (synopsis "Reimplementatin of core Koka, compiling to LLVM-IR, for my dissertation")
 (description "Reimplementatin of core Koka, compiling to LLVM-IR, for my dissertation")
 (depends
  (ocaml (>= 4.08.0))
  dune
  (alcotest :with-test)
  (odoc :with-doc)))
(using menhir 2.0)
