module One_of = struct
  let normalize (* TODO: should raise an error? *) x = x

  let restore = function
    (* Unbox variant value *)
    | `List [ `String _; value ] -> value
    | x -> x
end
