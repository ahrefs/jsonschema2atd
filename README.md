# jsonschema2atd

Generate [ATD](https://github.com/ahrefs/atd) types from a JSON Schema / OpenAPI document

## Installation

Install the package and dependencies with:
```bash
make install
```

## Usage

Generate ATD file out of JSON Schema
```bash
jsonschema2atd ../path-to-jsonschema.json
```

Generate ATD file out of OpenAPI document
```bash
jsonschema2atd --format openapi ../path-to-openapi.json
```

You can call `jsonschema2atd` and `atdgen` in your `dune` file to generate OCaml types and JSON serializers / desirializers from your JSON Scheme or OpenAPI document:
```
; add jsonschema2atd.runtime to have access to the oneOf serialization adapter (for variants unboxing)
(library
 ...
 (libraries ... jsonschema2atd.runtime))

; generate dashboard_gen.atd from dashboard_types_gen.json OpenAPI document with jsonschema2atd
(rule
 (target dashboard_gen.atd)
 ; store generated .atd file in the code 
 (mode promote)
 (deps ../grok/dashboard_types_gen.json)
 (action
  (with-stdout-to
   %{target}
   (run
    %{bin:jsonschema2atd} -f openapi
    %{deps}))))

; generate dashboard_gen_t.mli, dashboard_gen_t.ml, dashboard_gen_j.mli and dashboard_gen_j.ml from dashboard_gen.atd with atdgen
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
- [ ] allOf
