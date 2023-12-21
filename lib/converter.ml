open Json_schema_t
open Printf
open Utils

let record_field_name str =
  let cleaned_field_name = Utils.sanitize_name str in
  if String.equal str cleaned_field_name then str else sprintf {|%s <json name="%s">|} cleaned_field_name str

let define_top_level name type_ = sprintf "type %s = %s\n\n" (type_name name) type_

let process_int_type schema =
  match schema.format with
  | None | Some `Int32 -> "int"
  | Some `Int64 -> "int64"
  | _ -> failwith "int has unextected format"

let get_ref (ref : ref_) =
  match String.split_on_char '/' ref with
  | [ "#"; "components"; "schemas"; type_name ] -> type_name
  | _ -> failwith "only same file components refs is supported"

let rec ocaml_value_of_json = function
  | (`Bool _ | `Float _ | `Int _ | `Null) as json -> Yojson.Basic.to_string json
  | `String value -> sprintf "\\\"%s\\\"" value
  | `List elements ->
    let list_elements = List.map ocaml_value_of_json elements |> String.concat ";" in
    sprintf "[%s]" list_elements
  | `Assoc _ as json -> failwith (sprintf "can't make ocaml value from: %s" (Yojson.Basic.to_string json))

let make_atd_default_value enum json_value =
  match enum, json_value with
  | Some _, `String default_enum -> sprintf "`%s" (variant_name default_enum)
  | Some _, json ->
    failwith (sprintf "only string enum default values are supported, can't process: %s" (Yojson.Basic.to_string json))
  | None, json -> ocaml_value_of_json json

let nullable = Printf.sprintf "%s nullable"

let rec process_schema_type ~ancestors (schema : schema) =
  let maybe_nullable type_ = if schema.nullable then nullable type_ else type_ in
  match schema.one_of with
  | Some schemas -> process_one_of ~ancestors schemas
  | None ->
  match schema.enum, schema.typ with
  | Some enums, Some String -> process_enums enums
  | Some _, _ -> failwith "only string enums are supported"
  | None, _ ->
  match schema.typ with
  | Some Integer -> maybe_nullable (process_int_type schema)
  | Some Number -> maybe_nullable "float"
  | Some String -> maybe_nullable "string"
  | Some Boolean -> maybe_nullable "bool"
  | Some Array -> maybe_nullable (process_array_type ~ancestors schema |> String.concat " ")
  | Some Object ->
    if schema.nullable then process_nested_schema_type ~ancestors schema else process_object_type ~ancestors schema
  | None ->
    (* fallback to untyped if schema type is not defined *)
    maybe_nullable "json"

and process_array_type ~ancestors schema =
  match schema.items with
  | Some schema_or_ref -> [ make_type_from_schema_or_ref ~ancestors schema_or_ref; "list" ]
  | None -> failwith "items is not specified for array"

and toplevel_defenitions = Buffer.create 16

and process_nested_schema_type ~ancestors schema =
  match schema with
  | { one_of = Some _; _ } | { typ = Some Object; properties = Some _; _ } | { enum = Some _; _ } ->
    let nested_type_name = concat_camelCase (List.rev ancestors) in
    let nested = define_top_level nested_type_name (process_schema_type ~ancestors schema) in
    Buffer.add_string toplevel_defenitions nested;
    type_name nested_type_name
  | _ as schema -> process_schema_type ~ancestors schema

and process_object_type ~ancestors schema =
  let is_required field_name = List.exists (fun field -> String.equal field_name field) schema.required in
  let make_record_field (field_name, schema_or_ref) =
    let type_ = make_type_from_schema_or_ref ~ancestors:(field_name :: ancestors) schema_or_ref in
    let record_field_name = record_field_name field_name in
    match schema_or_ref, is_required field_name with
    | Obj { default = Some default; enum; _ }, _ ->
      sprintf "  ~%s <ocaml default=\"%s\">: %s;" record_field_name (make_atd_default_value enum default) type_
    | _, true -> sprintf "  %s: %s;" record_field_name type_
    | _, false -> sprintf "  ?%s: %s option;" record_field_name type_
  in
  match schema.properties with
  | Some properties -> sprintf "{\n%s\n}" (properties |> List.map make_record_field |> String.concat "\n")
  | None -> "json"

and make_type_from_schema_or_ref ~ancestors (schema_or_ref : schema or_ref) =
  match schema_or_ref, ancestors with
  | Obj schema, ([] | [ _ ]) -> process_schema_type ~ancestors schema
  | Obj schema, ancestors -> process_nested_schema_type ~ancestors schema
  | Ref ref_, _ -> type_name (get_ref ref_)

and process_one_of ~ancestors (schemas_or_refs : schema or_ref list) =
  let determine_variant_name = function
    | Ref ref_ -> variant_name (get_ref ref_)
    | Obj schema ->
    match schema.typ with
    | Some Array -> concat_camelCase (process_array_type ~ancestors schema)
    | Some Object -> "Json"
    | _ -> variant_name (process_schema_type ~ancestors schema)
  in
  let make_one_of_variant schema_or_ref =
    let variant_name = determine_variant_name schema_or_ref in
    sprintf "  | %s of %s" variant_name
      (make_type_from_schema_or_ref ~ancestors:(variant_name :: ancestors) schema_or_ref)
  in
  let variants = List.map make_one_of_variant schemas_or_refs |> String.concat "\n" in
  sprintf "[\n%s\n] <json adapter.ocaml=\"Openapi2atd_runtime.Adapter.One_of\">" variants

and process_enums enums =
  let make_enum_variant value = sprintf {|  | %s <json name="%s">|} (variant_name value) value in
  let variants = List.map make_enum_variant enums |> String.concat "\n" in
  sprintf "[\n%s\n]" variants

let process_schemas (schemas : (string * schema or_ref) list) =
  let atd_schemas =
    List.fold_left
      (fun acc (name, schema_or_ref) ->
        define_top_level name (make_type_from_schema_or_ref ~ancestors:[ name ] schema_or_ref) :: acc
      )
      [] schemas
  in
  String.concat "" (Buffer.contents toplevel_defenitions :: atd_schemas)

let prelude = {|
type json <ocaml module="Yojson.Basic" t="t"> = abstract

type int64 = int <ocaml repr="int64">
|}

let make_atd_of_json_scheme input =
  let schema = Json_schema_j.schema_of_string input in
  prelude ^ "\n" ^ process_schema_type ~ancestors:[] schema

let make_atd_of_openapi input =
  let root = Openapi_j.root_of_string input in
  match root.components with
  | None -> failwith "components are empty"
  | Some components ->
  match components.schemas with
  | Some schemas -> prelude ^ "\n" ^ process_schemas schemas
  | None -> failwith "components schemas are empty"
