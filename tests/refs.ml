open OUnit2
open Base

let simple_ref_test _ =
  let input =
    {|
{
  "one": {
    "type": "object",
    "properties": {
      "uid": {
        "type": "string"
      }
    }
  },
  "two": {
    "type": "object",
    "properties": {
      "datasource": {
        "$ref": "#/components/schemas/one"
      }
    }
  }
}
|}
  in
  let output =
    {|
    type two = {
      ?datasource: one option;
    }

    type one = {
      ?uid: string option;
    }

|}
  in
  assert_schema input output

let capitalized_refs_test _ =
  let input =
    {|
{
  "DataSourceRef": {
    "type": "object",
    "properties": {
      "uid": {
        "type": "string"
      }
    }
  },
  "RowPanel": {
    "type": "object",
    "properties": {
      "datasource": {
        "$ref": "#/components/schemas/DataSourceRef"
      }
    }
  }
}
|}
  in
  let output =
    {|
    type rowPanel = {
      ?datasource: dataSourceRef option;
    }

    type dataSourceRef = {
      ?uid: string option;
    }

|}
  in
  assert_schema input output

let suite = "Refs" >::: [ "simple ref" >:: simple_ref_test; "capitalized ref" >:: capitalized_refs_test ]

let () = run_test_tt_main suite
