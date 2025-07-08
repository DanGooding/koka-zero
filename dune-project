(lang dune 2.8)

(name koka-zero)

(license MIT)

(authors "Dan Gooding")

(maintainers "Dan Gooding")

(generate_opam_files true)

(package
 (name koka-zero)
 (synopsis "Reimplementatin of core Koka")
 (description
  "Reimplementatin of core Koka, compiling to LLVM-IR, for my dissertation")
 (depends
  (ocaml
   (>= 5.3.0))
  (core
   (and
    (>= v0.17)
    (< v.0.18)))
  (core_unix
   (and
    (>= v0.17)
    (< v.0.18)))
  dune
  (llvm
   (= 18-static))
  (menhir
   (and
    (>= 20240715)
    (< 20250101)))
  (ocamldot
   (and
    (>= 1.1)
    (< 1.2)))
  (ppx_jane
   (and
    (>= v0.17)
    (< v.0.18)))
  (ppx_expect :with-test)
  (odoc :with-doc)
  (ocaml-lsp-server :build)
  (ocamlformat :build)))

(using menhir 2.0)

;; don't require `dune__exe__` prefix when debugging
;; TODO: check the loss of namespacing is not an issue

(wrapped_executables false)

;; enable cram .t tests

(cram enable)
