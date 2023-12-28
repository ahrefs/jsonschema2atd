open OUnit2
open Base

let simple_test _ =
  let input =
    {| {
    "dummy": {
        "type": "object",
        "properties": {
            "id": {
                "type": "string"
            }
        },
        "allOf": [
            {
                "type": "object",
                "properties": {
                    "name": {
                        "type": "string"
                    }
                }
            },
            {
                "type": "object",
                "properties": {
                    "surname": {
                        "type": "string"
                    }
                }
            }
        ]
    }
} |}
  in
  let output =
    {|
    type dummy = {
      ?id: string option;
      ?name: string option;
      ?surname: string option;
     }
  |}
  in
  assert_schema input output

let suite = "allOf" >::: [ "simple test" >:: simple_test ]
let () = run_test_tt_main suite
