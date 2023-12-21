open OUnit2
open Base

let simple_test _ =
  let input =
    {| {
    "dummy": {
      "type": "object",
      "properties": {
        "field": {
          "id": "string"
        }
      },
      "allOf": [
        {
          "type": "object",
          "properties": {
            "field": {
              "name": "string"
            }
          }
        },
        {
          "type": "object",
          "properties": {
            "field": {
              "surname": "string"
            }
          }
        }
      ]
    }
}|}
  in
  let output =
    {|
    type dummy = {
      ?id: string option;
      ?name: string option;
      ?surname: string option;
  |}
  in
  assert_schema input output

let suite = "allOf" >::: [ "simple test" >:: simple_test ]
let () = run_test_tt_main suite
