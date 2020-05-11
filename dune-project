(lang dune 2.5.1)
(name ocaml-indexeddb)
(version 0.1.0)

(generate_opam_files true)

(source (github xguerin/ocaml-indexeddb))
(license ISC)
(authors "Thomas Leonard" "Xavier R. Guérin")
(maintainers "Xavier R. Guérin <github@applepine.org>")

(package
 (name ocaml-indexeddb)
 (synopsis "IndexedDB bindings for JSoO")
 (depends
  (dune (> 2.0))
  (js_of_ocaml (>= 3.0.0))
  (js_of_ocaml-lwt (>= 3.0.0))
  (js_of_ocaml-ppx (>= 3.0.0))
  (lwt (>= 5.0.0))))
