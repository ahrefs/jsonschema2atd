open Json_schema_t
open Printf
open Utils

let record_field_name str =
  let cleaned_field_name = Utils.sanitize_name str in
  if String.equal str cleaned_field_name then str else sprintf {|%s <json name="%s">|} cleaned_field_name str

let define_type name type_ = sprintf "type %s = %s\n" (type_name name) type_

let process_int_type schema =
  match schema.format with
  | None | Some `Int32 -> "int"
  | Some `Int64 -> "int64"
  | _ -> failwith "int has unextected format"

let get_ref_name ref =
  match String.split_on_char '/' ref with
  (* OpenAPI defs *)
  | [ "#"; "components"; "schemas"; type_name ] -> type_name
  (* JSON Schema defs *)
  | [ "#"; "$defs"; type_name ] -> type_name
  | _ ->
    failwith
      (Printf.sprintf "Unsupported ref value: %s. Supported ref URI are: #/components/schemas/* and #/$defs/*" ref)

let output = Buffer.create 16
let input_toplevel_schemas = ref []

let get_schema_by_ref ~schema ref =
  let defs =
    match schema.defs with
    | None -> !input_toplevel_schemas
    | Some defs -> defs @ !input_toplevel_schemas
  in
  List.find_map
    (function
      | name, schema when String.equal (get_ref_name ref) name -> Some schema
      | _ -> None
      )
    defs

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

let merge_all_of schema =
  match schema.all_of with
  | None -> schema
  | Some [] -> failwith "empty allOf is unexpected"
  | Some all_of ->
    let ref_schemas =
      List.filter_map
        (function
          | Obj schema -> Some schema
          | Ref ref -> get_schema_by_ref ~schema ref
          )
        all_of
    in
    let schemas = schema :: ref_schemas in
    let take_first_opt get_fn =
      match schemas |> List.filter_map get_fn with
      | [] -> None
      | first :: _ -> Some first
    in
    let merge_lists get_fn = schemas |> List.map get_fn |> List.flatten in
    let merge_opt_lists get_fn = schemas |> List.filter_map get_fn |> List.flatten |> Utils.nonempty_list_opt in
    {
      schema = take_first_opt (fun schema -> schema.schema);
      all_of = merge_opt_lists (fun schema -> schema.all_of);
      any_of = merge_opt_lists (fun schema -> schema.any_of);
      one_of = merge_opt_lists (fun schema -> schema.one_of);
      not = take_first_opt (fun schema -> schema.not);
      items = take_first_opt (fun schema -> schema.items);
      properties = merge_opt_lists (fun schema -> schema.properties);
      additional_properties = take_first_opt (fun schema -> schema.additional_properties);
      enum =
        schemas
        |> List.filter_map (fun schema -> schema.enum)
        |> Utils.shortest_list
        |> Option.value ~default:[]
        |> Utils.nonempty_list_opt;
      max_length = take_first_opt (fun schema -> schema.max_length);
      min_length = take_first_opt (fun schema -> schema.min_length);
      pattern = take_first_opt (fun schema -> schema.pattern);
      max_items = take_first_opt (fun schema -> schema.max_items);
      min_items = take_first_opt (fun schema -> schema.min_items);
      unique_items = take_first_opt (fun schema -> schema.unique_items);
      max_contains = take_first_opt (fun schema -> schema.max_contains);
      min_contains = take_first_opt (fun schema -> schema.min_contains);
      max_properties = take_first_opt (fun schema -> schema.max_properties);
      min_properties = take_first_opt (fun schema -> schema.min_properties);
      required = merge_lists (fun schema -> schema.required);
      dependent_required = merge_lists (fun schema -> schema.dependent_required);
      format = take_first_opt (fun schema -> schema.format);
      defs = merge_opt_lists (fun schema -> schema.defs);
      title = take_first_opt (fun schema -> schema.title);
      typ = take_first_opt (fun schema -> schema.typ);
      description = take_first_opt (fun schema -> schema.description);
      default = take_first_opt (fun schema -> schema.default);
      nullable = schemas |> List.exists (fun schema -> schema.nullable);
    }

let rec process_schema_type ~ancestors (schema : schema) =
  let schema = merge_all_of schema in
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
  | Some Object -> process_object_type ~ancestors schema
  | None ->
    (* fallback to untyped if schema type is not defined *)
    maybe_nullable "json"

and process_array_type ~ancestors schema =
  match schema.items with
  | Some schema_or_ref -> [ make_type_from_schema_or_ref ~ancestors schema_or_ref; "list" ]
  | None -> failwith "items is not specified for array"

and process_nested_schema_type ~ancestors schema =
  match merge_all_of schema with
  | { one_of = Some _; _ } | { typ = Some Object; properties = Some _; _ } | { enum = Some _; _ } ->
    let nested_type_name = concat_camelCase (List.rev ancestors) in
    let nested = define_type nested_type_name (process_schema_type ~ancestors schema) in
    Buffer.add_string output (nested ^ "\n");
    type_name nested_type_name
  | _ -> process_schema_type ~ancestors schema

and process_object_type ~ancestors schema =
  let is_required field_name = List.exists (String.equal field_name) schema.required in
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
  | Ref ref_, _ -> type_name (get_ref_name ref_)

and process_one_of ~ancestors (schemas_or_refs : schema or_ref list) =
  let determine_variant_name = function
    | Ref ref_ -> variant_name (get_ref_name ref_)
    | Obj schema ->
    match (merge_all_of schema).typ with
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
  sprintf "[\n%s\n] <json adapter.ocaml=\"Jsonschema2atd_runtime.Adapter.One_of\">" variants

and process_enums enums =
  let make_enum_variant value = sprintf {|  | %s <json name="%s">|} (variant_name value) value in
  let variants = List.map make_enum_variant enums |> String.concat "\n" in
  sprintf "[\n%s\n]" variants

let process_schemas (schemas : (string * schema or_ref) list) =
  List.fold_left
    (fun acc (name, schema_or_ref) ->
      define_type name (make_type_from_schema_or_ref ~ancestors:[ name ] schema_or_ref) :: acc
    )
    [] schemas

let base =
  {|(* Generated by jsonschema2atd *)
type json <ocaml module="Yojson.Basic" t="t"> = abstract
type int64 = int <ocaml repr="int64">
|}

let make_atd_of_schemas schemas =
  input_toplevel_schemas :=
    List.filter_map
      (function
        | _name, Ref _ -> None
        | name, Obj schema -> Some (name, schema)
        )
      schemas;
  Buffer.clear output;
  Buffer.add_string output (base ^ "\n");
  Buffer.add_string output (String.concat "\n" (process_schemas schemas));
  Buffer.contents output

let make_atd_of_jsonschema input =
  let schema = Json_schema_j.schema_of_string input in
  let root_type_name = Option.value ~default:"root" schema.title in
  make_atd_of_schemas [ root_type_name, Obj schema ]

let make_atd_of_openapi input =
  let root = Openapi_j.root_of_string input in
  match root.components with
  | None -> failwith "components are empty"
  | Some components ->
  match components.schemas with
  | Some schemas -> make_atd_of_schemas schemas
  | None -> failwith "components schemas are empty"
