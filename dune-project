(lang dune 3.20)

(name sokoban)

(generate_opam_files true)

(source
 (github jrfondren/sokoban))

(authors "Julian Fondren <julian.fondren@gmail.com>")

(maintainers "Julian Fondren <julian.fondren@gmail.com>")

(license MIT)

(documentation https://github.com/jrfondren/sokoban)

(package
 (name sokoban)
 (synopsis "A toy implementation of Sokoban")
 (description "A game about pushing blocks.")
 (depends ocaml terml)
 (tags
  ("game")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
