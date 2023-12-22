Generate ATD out of JSON Scheme
  $ jsonschema2atd ./mocks/simple_jsonschema.json
  
  type json <ocaml module="Yojson.Basic" t="t"> = abstract
  type int64 = int <ocaml repr="int64">
  
  type product = {
    ?productId: int option;
  }
  
Generate ATD out of JSON Scheme with --format attribute
  $ jsonschema2atd --format jsonschema ./mocks/simple_jsonschema.json
  
  type json <ocaml module="Yojson.Basic" t="t"> = abstract
  type int64 = int <ocaml repr="int64">
  
  type product = {
    ?productId: int option;
  }
  
Generate ATD out of JSON Scheme with -f attribute
  $ jsonschema2atd -f jsonschema ./mocks/simple_jsonschema.json
  
  type json <ocaml module="Yojson.Basic" t="t"> = abstract
  type int64 = int <ocaml repr="int64">
  
  type product = {
    ?productId: int option;
  }
  
Generate ATD out of OpenAPI doc with --format attribute
  $ jsonschema2atd --format openapi ./mocks/simple_openapi.json
  
  type json <ocaml module="Yojson.Basic" t="t"> = abstract
  type int64 = int <ocaml repr="int64">
  
  type product = {
    ?productId: int option;
  }
  
Generate ATD out of OpenAPI doc with -f attribute
  $ jsonschema2atd -f openapi ./mocks/simple_openapi.json
  
  type json <ocaml module="Yojson.Basic" t="t"> = abstract
  type int64 = int <ocaml repr="int64">
  
  type product = {
    ?productId: int option;
  }
  