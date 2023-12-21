open OUnit2
open Base

let simple_test _ =
  let input =
    {| {
    "dummy": {
      "oneOf": [
        {
          "type": "string"
        },
        {
          "type": "array",
          "items": {
            "type": "string"
          }
        }
      ]
    }
}|}
  in
  let output =
    {|
  type dummy = [
    | String of string
    | StringList of string list
  ] <json adapter.ocaml="Openapi2atd_runtime.Adapter.One_of">
|}
  in
  assert_schema input output

let nested_one_of_test _ =
  let input =
    {| {
    "VariableModel": {
      "type": "object",
      "properties": {
        "query": {
          "description": "Query used to fetch values for a variable",
          "oneOf": [
            {
              "type": "string"
            },
            {
              "type": "object",
              "properties": {
                "inline": {
                  "type": "string"
                }
              }
            }
          ]
        }
      }
    }
}|}
  in
  let output =
    {|
    type variableModelQueryJson = {
      ?inline: string option;
    }

    type variableModelQuery = [
      | String of string
      | Json of variableModelQueryJson
    ] <json adapter.ocaml="Openapi2atd_runtime.Adapter.One_of">

    type variableModel = {
      ?query: variableModelQuery option;
    }|}
  in
  assert_schema input output

let suite = "oneOf" >::: [ "simple" >:: simple_test; "nested one of test" >:: nested_one_of_test ]
let () = run_test_tt_main suite
