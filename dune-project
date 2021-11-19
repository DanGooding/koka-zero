(lang dune 2.0)

;; don't require `dune__exe__` prefix when debugging
;; TODO: check the loss of namespacing is not an issue
(wrapped_executables false)

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
  (ppx_expect :with-test)
  (odoc :with-doc)))
(using menhir 2.0)

