open Cmdliner

let convert path_in =
  let ic = open_in path_in in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let atd = Converter.make_atd_of_openapi content in

  print_string atd;
  ()

let convert_cmd =
  let doc = "Convert OpenAPI schema to atdgen types" in
  let path_in = Arg.(required & pos 0 (some string) None & info [] ~docv:"OpenAPI .json file path" ~doc) in
  let term = Term.(const convert $ path_in) in
  let info = Cmd.info "convert" ~doc in
  Cmd.v info term

let () = exit (Cmd.eval convert_cmd)
