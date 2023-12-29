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


let with_nested _ =
  let input =
    {| {
      "MappingType": {
        "type": "string",
        "enum": [
          "value",
          "range",
          "regex",
          "special"
        ]
      },
      "RangeMap": {
        "type": "object",
        "required": [
          "type",
          "options"
        ],
        "properties": {
          "type": {
            "type": "string",
            "allOf": [
              {
                "$ref": "#/components/schemas/MappingType"
              },
              {
                "enum": [
                  "range"
                ]
              }
            ]
          }
        }
      }
} |}
  in
  let output =
    {|
    type rangeMapType = [
      | Range <json name="range">
    ]

    type rangeMap = {
      type_ <json name="type">: rangeMapType;
    }

    type mappingType = [
      | Value <json name="value">
      | Range <json name="range">
      | Regex <json name="regex">
      | Special <json name="special">
    ]
  |}
  in
  assert_schema input output

let suite = "allOf" >::: [
  "simple test" >:: simple_test;
  "with nested" >:: with_nested
  ]
let () = run_test_tt_main suite
