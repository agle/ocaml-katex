(lang dune 3.22)

(name ocaml-katex)

(generate_opam_files true)

(source
 (github agle/ocaml-katex))

(authors "Alicia M <a.michael1@uq.edu.au>")

(maintainers "Alicia M <a.michael1@uq.edu.au>")

(license BSD-2-Clause)

(documentation https://github.com/agle/ocaml-katex)

(package
 (name ocaml-katex)
 (synopsis "Offline KaTeX transform for cmarkit")
 (description "Offline KaTeX transform for cmarkit based on QuickJS")
 (depends 
   cmarkit
   ocaml
   ppx_blob
   quickjs_eval)
 (tags
  ("latex", "html", "markdown")))
