open OUnit2
open Base

let simple_test _ =
  let input = {|{"dummy":{"type":"object","properties":{"id":{"type":"integer"},"name":{"type":"string"}}}}|} in
  let output = {|
    type dummy = {
      ?id: int option;
      ?name: string option;
    }|} in
  assert_schema input output

let capitalized_props_test _ =
  let input =
    {|{
    "dummy": {
    "type": "object",
    "properties": {
      "Id": {
        "type": "integer"
      },
      "Name": {
        "type": "string"
      }
    }
  }}|}
  in
  let output =
    {|
    type dummy = {
      ?id <json name="Id"> : int option;
      ?name <json name="Name"> : string option;
    }|}
  in
  assert_schema input output

let with_required_fields _ =
  let input =
    {|{
  "dummy": {
    "type": "object",
    "properties": {
      "id": {
        "type": "integer"
      },
      "name": {
        "type": "string"
      },
      "surname": {
        "type": "string"
      }
    },
    "required": [
      "id",
      "name"
    ]
  }
}|}
  in
  let output = {|
    type dummy = {
      id: int;
      name: string;
      ?surname: string option;
    }|} in
  assert_schema input output

let with_reserved_keyword_fieldname _ =
  let input = {|{"dummy":{"type":"object","properties":{"id":{"type":"integer"},"type":{"type":"string"}}}}|} in
  let output = {|
    type dummy = {
      ?id: int option;
      ?type_ <json name="type">: string option;
    }|} in
  assert_schema input output

let two_objects_with_same_nested_object_field_names _ =
  let input =
    {|{
      "one": {
        "type": "object",
        "required": ["value"],
        "properties": {
          "value": {
            "oneOf": [
              {
                "type": "string"
              },
              {
                "type": "integer"
              }
            ]
          }
        }
      },
      "two": {
        "type": "object",
        "required": ["value"],
        "properties": {
          "value": {
            "oneOf": [
              {
                "type": "string"
              },
              {
                "type": "integer"
              }
            ]
          }
        }
      }
    }|}
  in
  let output =
    {|
    type oneValue = [
      | String of string
      | Int of int
    ] <json adapter.ocaml="Openapi2atd_runtime.Adapter.One_of">

    type twoValue = [
      | String of string
      | Int of int
    ] <json adapter.ocaml="Openapi2atd_runtime.Adapter.One_of">

    type two = {
      value: twoValue;
    }

    type one = {
      value: oneValue;
    }
    |}
  in
  assert_schema input output

let with_nested_object _ =
  let input =
    {|{
  "dummy": {
    "type": "object",
    "properties": {
      "prop1": {
        "type": "object",
        "properties": {
          "nested_prop": {
            "type": "integer"
          }
        }
      }
    }
  }
}|}
  in
  let output =
    {|
    type dummyProp1 = {
     ?nested_prop: int option;
    }

    type dummy = {
      ?prop1: dummyProp1 option;
    }|}
  in
  assert_schema input output

let suite =
  "Objects"
  >::: [
         "simple" >:: simple_test;
         "capitalized props" >:: capitalized_props_test;
         "with required fields" >:: with_required_fields;
         "with reserved keyword fieldname" >:: with_reserved_keyword_fieldname;
         "with nested object" >:: with_nested_object;
         "two objects with same nested object field names" >:: two_objects_with_same_nested_object_field_names;
       ]

let () = run_test_tt_main suite
