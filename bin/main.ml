open Cmdliner

module Input_format = struct
  type t =
    | JSONSchema
    | OpenAPI

  let stringify = function
    | JSONSchema -> "jsonschema"
    | OpenAPI -> "openapi"

  let all = [ JSONSchema; OpenAPI ]
end

let generate_atd input_format path_in =
  let ic = open_in path_in in
  let input_content = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let generate =
    match input_format with
    | Input_format.JSONSchema -> Generator.make_atd_of_jsonschema
    | OpenAPI -> Generator.make_atd_of_openapi
  in
  input_content |> generate |> print_string;
  ()

let input_format_term =
  let formats = List.map (fun fmt -> Input_format.stringify fmt, fmt) Input_format.all in
  let doc = Printf.sprintf "Input file format. Available options: %s" (List.map fst formats |> String.concat ", ") in
  let format = Arg.(enum formats) in
  Arg.(value & opt format JSONSchema & info [ "format"; "f" ] ~docv:"FORMAT" ~doc)

let main =
  let doc = "Generate ATD types from a JSON Schema / OpenAPI document" in
  let path_in = Arg.(required & pos 0 (some file) None & info [] ~docv:"input file" ~doc) in
  let term = Term.(const generate_atd $ input_format_term $ path_in) in
  let info = Cmd.info "jsonschema2atd" ~doc ~version:(Version.get ()) in
  Cmd.v info term

let () = exit (Cmd.eval main)
