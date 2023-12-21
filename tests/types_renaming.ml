open OUnit2
open Base

let dot_in_the_file_names _ =
  let input = {|{"status.OperatorState": {"type": "string"}}|} in
  let output = "type statusOperatorState = string" in
  assert_schema input output

let suite = "Types renaming" >::: [ "dot_in_the_file_names" >:: dot_in_the_file_names ]
let () = run_test_tt_main suite
