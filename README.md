# jsonschema2atd

Generates [ATD](https://github.com/ahrefs/atd) types based on the JSON Scheme / OpenAPI document

## Setup and build

Install opam dependencies

```bash
make install
```

Build the project

```bash
make
```

Run converter

```bash
opam exec -- dune exec -- ./jsonschema2atd.exe ~/path-to-openapi-scheme
```

See all available targets with `make help`.

## OpenAPI scheme features support

- [X] Base types
- [X] Records
- [X] Nullable
- [X] String enums
- [ ] Integer enums
- [ ] Other base type enums
- [X] Refs
- [X] OneOf
- [ ] not
- [ ] anyOf
- [ ] allOf
