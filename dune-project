(lang dune 3.10)

(using menhir 2.1)

(name mlsus)

(source
 (github johnyob/mlsus))

(authors "Alistair O'Brien")

(maintainers "ajo41@cam.ac.uk")

(license MIT)

(package
 (name mlsus)
 (depends ocaml dune core ppx_jane fmt menhir))
