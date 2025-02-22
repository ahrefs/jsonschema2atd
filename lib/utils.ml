let reserved_keywords =
  [
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let is_keyword word = List.mem word reserved_keywords

let sanitize_name str =
  let str' = String.uncapitalize_ascii str in
  let str' = Str.global_replace (Str.regexp "[^a-zA-Z_1-9]") "" str' in
  if is_keyword str' then str' ^ "_" else str'

let concat_camelCase strs = strs |> List.map String.capitalize_ascii |> String.concat ""
let concat_snake_case strs = String.concat "_" strs
let type_name = sanitize_name

let variant_name str =
  match str with
  | "" -> "None"
  | _ ->
  match str.[0] with
  | '0' .. '9' -> "N_" ^ sanitize_name str
  | _ -> String.capitalize_ascii (sanitize_name str)

module Fresh (T : sig
  type t

  val compare : t -> t -> int
end)
() =
struct
  external id : 'a -> 'a = "%identity"

  type t = T.t

  let inject = id
  let project = id
  let inject_list = id
  let project_list = id
  let compare = T.compare
  let equal a b = T.compare a b = 0
end

let hd_opt = function
  | [] -> None
  | first :: _ -> Some first

let shortest_list lists = lists |> List.sort (fun a b -> compare (List.length a) (List.length b)) |> hd_opt

let nonempty_list_opt = function
  | [] -> None
  | non_empty_list -> Some non_empty_list

let list_of_nonempty = function
  | None -> []
  | Some l -> l
