(lang dune 2.0)
(name brands)

(generate_opam_files true)
(source (github CraigFe/brands))
(maintainers "Craig Ferguson <me@craigfe.io>")
(authors "Craig Ferguson <me@craigfe.io>")

(package
 (name brands)
 (synopsis "An implementation of defunctionalised higher-kinded types in OCaml.")
 (documentation https://CraigFe.github.io/kind/))

(package
 (name ppx_brands)
 (synopsis "Derive type brands for higher-kinded polymorphism")
 (documentation https://CraigFe.github.io/sink/)
 (depends
  (ppxlib (>= 0.15.0))))