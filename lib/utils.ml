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

let variant_name str = String.capitalize_ascii (sanitize_name str)
