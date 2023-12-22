# jsonschema2atd

Generates [ATD](https://github.com/ahrefs/atd) types based on the JSON Schema / OpenAPI document.

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

## OpenAPI schema features support

- [X] Base types
- [X] Records
- [X] Nullable
- [X] String enums
- [ ] Integer enums
- [ ] Other primitive enums
- [X] Refs (OpenAPI format)
- [X] OneOf (stringification only)
- [ ] not
- [ ] anyOf
- [ ] allOf
