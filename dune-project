(lang dune 3.10)

(name WritingACompilerInGOCaml)

(generate_opam_files true)

(source
 (github toasted-cornkernels/WritingACompilerInGOCaml))

(authors "ToastedCornkernels")

(maintainers "ToastedCornkernels")

(license LICENSE)

(package
 (name WritingACompilerInGOCaml)
 (synopsis "MonkeyLang compiler in OCaml")
 (description "Port of <Writing a compiler in Go> in OCaml")
 (depends ocaml dune utop)
 (tags
  (topics "compiler")))
