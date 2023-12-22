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
