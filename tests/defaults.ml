open OUnit2
open Base

let simple_test _ =
  let input =
    {|{
    "dummy": {
    "type": "object",
    "required": ["id"],
    "properties": {
      "id": {
        "type": "string"
      },
      "str_value": {
        "type": "string",
        "default": "dummy default_string"
      },
      "int_value": {
        "type": "integer",
        "default": 12345
      },
      "bool_value": {
        "type": "boolean",
        "default": true
      }
    }
  }}|}
  in
  let output =
    {|
    type dummy = {
      id: string;
      ~str_value <ocaml default="\"dummy default_string\"">: string;
      ~int_value <ocaml default="12345">: int;
      ~bool_value <ocaml default="true">: bool;
    }|}
  in
  assert_schema input output

let suite = "Object with defaults" >::: [ "simple test" >:: simple_test ]
let () = run_test_tt_main suite
