# jsonschema2atd

Generate an [ATD](https://github.com/ahrefs/atd) file from a JSON Schema / OpenAPI document.

## Installation

The package is available on [opam](https://ocaml.org/p/jsonschema2atd/latest).

```
opam install jsonschema2atd
```

If you wish to install the development version you can do so with:

```bash
make install
```

## Usage

Generate an ATD file from a JSON Schema:
```bash
jsonschema2atd ../path-to-jsonschema.json
```

Generate an ATD file from an OpenAPI document:
```bash
jsonschema2atd --format openapi ../path-to-openapi.json
```

You can call `jsonschema2atd` and `atdgen` in your `dune` file to generate OCaml types and JSON serializers/deserializers from your JSON Schema or OpenAPI document:
```
; Add jsonschema2atd.runtime to have access to the oneOf serialization adapter (for variant unboxing).
(library
 ...
 (libraries ... jsonschema2atd.runtime))

; Generate dashboard_gen.atd from the dashboard_types_gen.json OpenAPI document with jsonschema2atd.
(rule
 (target dashboard_gen.atd)
 ; Store the generated .atd file in the code. 
 (mode promote)
 (deps ../grok/dashboard_types_gen.json)
 (action
  (with-stdout-to
   %{target}
   (run
    %{bin:jsonschema2atd} -f openapi
    %{deps}))))

; Generate dashboard_gen_t.mli, dashboard_gen_t.ml, dashboard_gen_j.mli, and dashboard_gen_j.ml from dashboard_gen.atd with atdgen.
(rule
 (targets
  dashboard_gen_t.mli
  dashboard_gen_t.ml
  dashboard_gen_j.mli
  dashboard_gen_j.ml)
 (deps dashboard_gen.atd)
 (action
  (progn
   (run %{bin:atdgen} -j -j-std -j-defaults %{deps})
   (run %{bin:atdgen} -t %{deps}))))
```

Other options can be used to control the output:

- `--json-ocaml-type KEYWORD:MODULE.PATH:TYPE-NAME` to control the defitiion of
  the `json` type used as default/fallback.
- `--only-matching REGEXP` to limit the JSONSchema types to convert, when used
  together with `--avoid-dangling-refs`, missing types are replaced with `json`.

See also `jsonschema2atd --help`.

## ToDo

- [X] Base types
- [X] Records
- [X] Nullable
- [X] String enums
- [ ] Integer enums
- [ ] Other primitive enums
- [X] Refs (OpenAPI format)
- [X] OneOf (Only serialization is supported)
- [ ] not
- [ ] anyOf
- [X] allOf
