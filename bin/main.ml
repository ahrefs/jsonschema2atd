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

let generate_atd input_format paths =
  let generate =
    match input_format with
    | Input_format.JSONSchema -> Generator.make_atd_of_jsonschema
    | OpenAPI -> Generator.make_atd_of_openapi
  in
  print_endline (Generator.base (String.concat " " (List.map Filename.basename paths)));
  let root =
    match paths with
    | [ _ ] -> `Default
    | _ -> `Per_file
  in
  List.iter
    (fun path ->
      let root =
        match root with
        | `Default -> None
        | `Per_file -> Some (path |> Filename.basename |> Filename.remove_extension |> Utils.sanitize_name)
      in
      let input_content = In_channel.with_open_bin path In_channel.input_all in
      input_content |> generate ?root |> print_string
    )
    paths

let input_format_term =
  let formats = List.map (fun fmt -> Input_format.stringify fmt, fmt) Input_format.all in
  let doc = Printf.sprintf "Input file format. Available options: %s" (List.map fst formats |> String.concat ", ") in
  let format = Arg.(enum formats) in
  Arg.(value & opt format JSONSchema & info [ "format"; "f" ] ~docv:"FORMAT" ~doc)

let main =
  let doc = "Generate an ATD file from a list of JSON Schema / OpenAPI document" in
  let paths = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES" ~doc) in
  let term = Term.(const generate_atd $ input_format_term $ paths) in
  let info = Cmd.info "jsonschema2atd" ~doc ~version:(Version.get ()) in
  Cmd.v info term

let () = exit (Cmd.eval main)
