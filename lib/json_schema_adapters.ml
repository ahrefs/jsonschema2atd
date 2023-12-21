module Or_ref = struct
  let normalize = function
    | `Assoc [ ("$ref", ref) ] -> `List [ `String "Ref"; ref ]
    | obj -> `List [ `String "Obj"; obj ]

  let restore = function
    | `List [ `String "Ref"; ref ] -> `Assoc [ "$ref", ref ]
    | `List [ `String "Obj"; obj ] -> obj
    | x -> x
end

module Ref = Utils.Fresh (String) ()
