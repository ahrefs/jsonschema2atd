open OUnit2

let openapi_json_template schemas =
  Printf.sprintf
    {|{"openapi": "3.0.0", "info": { "title": "dashboard", "version": "0.0" }, "paths": {}, "components": { "schemas": %s }}|}
    schemas

let replace_whitespace str = Str.global_replace (Str.regexp "[ \t\n\r]+") "" str

let remove_prelude str = Str.global_replace (Str.regexp Converter.prelude) "" str

let test_strings_cmp a b = String.equal (replace_whitespace a) (replace_whitespace b)

let assert_schema input output =
  assert_equal ~cmp:test_strings_cmp
    ~printer:(fun str -> str)
    output
    (remove_prelude (Converter.make_atd_of_openapi (openapi_json_template input)))
