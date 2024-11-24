module Or_ref = struct
  let normalize = function
    | `Assoc [ ("$ref", ref) ] -> `List [ `String "Ref"; ref ]
    | obj -> `List [ `String "Obj"; obj ]

  let restore = function
    | `List [ `String "Ref"; ref ] -> `Assoc [ "$ref", ref ]
    | `List [ `String "Obj"; obj ] -> obj
    | x -> x
end

module Or_bool = struct
  let normalize : Yojson.Safe.t -> Yojson.Safe.t = function
    | `Bool b -> `List [ `String "Bool"; `Bool b ]
    | obj -> `List [ `String "Obj"; obj ]

  let restore = function
    | `List [ `String "Bool"; `Bool b ] -> `Bool b
    | `List [ `String "Obj"; obj ] -> obj
    | x -> x
end

module Ref = Utils.Fresh (String) ()
