open OUnit2
open Base

let nullable_integer_test _ =
  let input = {|{"dummy": { "type": "integer", "nullable": true}}|} in
  let output = "type dummy = int nullable" in
  assert_schema input output

let suite = "Nullable" >::: [ "nullable integer test" >:: nullable_integer_test ]
let () = run_test_tt_main suite
