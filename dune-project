(lang dune 3.0)
(generate_opam_files true)

(name jsonschema2atd)
(version "0.0.1")
(source (github ahrefs/jsonschema2atd))
(authors "Ahrefs")
(maintainers "Egor Chemokhonenko <egor.chemohonenko@ahrefs.com>")
(license "MIT")
(package
 (name jsonschema2atd)
 (synopsis "Generate ATD types out of JSON Schema and OpenAPI document")
 (description "Generate ATD types out of JSON Schema and OpenAPI document")
 (depends
  (ocaml (>= 4.10))
  (atdgen (>= 2.7))
  (atdgen-runtime (>= 2.0))
  (cmdliner (>= 1.1.0))
  dune-build-info
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat (and :with-dev-setup (= 0.24.1)))
  (odoc :with-doc)
  (ounit2 :with-test)
  yojson))
