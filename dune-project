(lang dune 2.8)

(name koka-zero)

(license MIT)

(authors
  "Dan Gooding")

(maintainers
  "Dan Gooding")

(generate_opam_files false)

(package
 (name koka-zero)
 (synopsis "Reimplementatin of core Koka")
 (description "Reimplementatin of core Koka, compiling to LLVM-IR, for my dissertation")
 (depends
  (ocaml (>= 4.08.0))
  core
  dune
  (llvm (= 18-static))
  ppx_jane
  (ppx_expect :with-test)
  (odoc :with-doc)))

(using menhir 2.0)

;; don't require `dune__exe__` prefix when debugging
;; TODO: check the loss of namespacing is not an issue
(wrapped_executables false)

;; enable cram .t tests
(cram enable)
