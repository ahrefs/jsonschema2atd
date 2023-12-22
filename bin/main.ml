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

let convert format path_in =
  let ic = open_in path_in in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let atd =
    match format with
    | Input_format.JSONSchema -> Converter.make_atd_of_jsonschema content
    | OpenAPI -> Converter.make_atd_of_openapi content
  in

  print_string atd;
  ()

let input_format_term =
  let formats = List.map (fun fmt -> Input_format.stringify fmt, fmt) Input_format.all in
  let doc = Printf.sprintf "Input file format. Available options: %s" (List.map fst formats |> String.concat ", ") in
  let format = Arg.(enum formats) in
  Arg.(value & opt format JSONSchema & info [ "format"; "f" ] ~docv:"FORMAT" ~doc)

let convert_cmd =
  let doc = "Generate ATD types from a JSON schema / OpenAPI document" in
  let path_in = Arg.(required & pos 0 (some file) None & info [] ~docv:"input file" ~doc) in
  let term = Term.(const convert $ input_format_term $ path_in) in
  let info = Cmd.info "jsonschema2atd" ~doc in
  Cmd.v info term

let () = exit (Cmd.eval convert_cmd)
