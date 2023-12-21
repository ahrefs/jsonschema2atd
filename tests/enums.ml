open OUnit2
open Base

let top_level_enum _ =
  let input =
    {|{
    "dummy": {
      "type": "string",
      "enum": [
        "success",
        "in_progress",
        "failed"
      ]
    }
  }|}
  in
  let output =
    {|
    type dummy = [
      | Success <json name="success">
      | In_progress <json name="in_progress">
      | Failed <json name="failed">
    ]

  |}
  in
  assert_schema input output

let nested_enum _ =
  let input =
    {|{
    "dummy": {
      "type": "object",
      "properties": {
        "nested": {
          "type": "string",
          "enum": [
            "success",
            "in_progress",
            "failed"
          ]
        }
      }
    }
  }|}
  in
  let output =
    {|
    type dummyNested = [
      | Success <json name="success">
      | In_progress <json name="in_progress">
      | Failed <json name="failed">
    ]

    type dummy = {
      ?nested: dummyNested option;
    }

  |}
  in
  assert_schema input output

let suite = "Nullable" >::: [ "top level enum" >:: top_level_enum; "nested enum" >:: nested_enum ]
let () = run_test_tt_main suite
