open OUnit2
open Base

let int_64_test _ =
  let input = {|{"dummy": { "type": "integer", "format": "int64"}}|} in
  let output = "type dummy = int64" in
  assert_schema input output

let int_32_test _ =
  let input = {|{"dummy": { "type": "integer", "format": "int32"}}|} in
  let output = "type dummy = int" in
  assert_schema input output

let number_test _ =
  let input = {|{"dummy": { "type": "number"}}|} in
  let output = "type dummy = float" in
  assert_schema input output

let boolean_test _ =
  let input = {|{"dummy": { "type": "boolean"}}|} in
  let output = "type dummy = bool" in
  assert_schema input output

let string_test _ =
  let input = {|{"dummy": { "type": "string"}}|} in
  let output = "type dummy = string" in
  assert_schema input output

let array_test _ =
  let input = {|{"dummy":{"type":"array","items":{"type":"string"}}}|} in
  let output = {|type dummy = string list|} in
  assert_schema input output

let suite =
  "Primitives"
  >::: [
         "int 32" >:: int_32_test;
         "int 64" >:: int_64_test;
         "number" >:: number_test;
         "boolean" >:: boolean_test;
         "string" >:: string_test;
         "array" >:: array_test;
       ]

let () = run_test_tt_main suite
