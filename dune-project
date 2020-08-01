(lang dune 1.10)

(name cmph)
(license MIT)
(authors "Jack Feser <feser@csail.mit.edu>")
(maintainers "Jack Feser <feser@csail.mit.edu>")
(source (github jfeser/ocaml-cmph))
(documentation https://jfeser.github.io/ocaml-cmph/)

(package
 (name cmph)
 (synopsis "OCaml wrapper for the C Minimal Perfect Hashing library")
 (description "ocaml-cmph is a wrapper library for the C Minimal Perfect Hashing (CMPH)
library. CMPH contains algorithms for generating perfect hashes. ocaml-cmph
exposes a high level interface to these hash generators.
")
 (depends
  (dune (and :build (>= 1.10)))
  (ounit (and :with_test (>= 2.0)))
  (ctypes (>= 0.14))
  (ctypes-foreign (>= 0.4))
  (core (>= v0.14))
  (ppx_sexp_conv (>= v0.14))))

(using fmt 1.1)
(generate_opam_files true)

; Local Variables:
; mode: dune
; End:
