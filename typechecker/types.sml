structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (unit -> (Symbol.symbol * ty) list) * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | ARROW of ty list * ty (* function type: arguments * return *)
	        | UNIT
          | BOTTOM

  fun tostring(RECORD(fields)) = "record" 
    | tostring(NIL) = "nil"
    | tostring(INT) = "int"
    | tostring(STRING) = "string"
    | tostring(ARRAY(fields)) = "array"
    | tostring(ARROW(fields)) = "arrow"
    | tostring(UNIT) = "unit"
    | tostring(BOTTOM) = "bottom"

  fun are_the_same_type(ty1, ty2) = (* ty * ty -> bool *)
      case (ty1, ty2) of
          (INT, INT) => true
        | (NIL, NIL) => true
        | (STRING, STRING) => true
        | (UNIT, UNIT) => true
        | (BOTTOM, BOTTOM) => true
        | (ARRAY(_, u1), ARRAY(_, u2)) => u1 = u2
        | (RECORD(_, u1), RECORD(_, u2)) => if u1 = u2 then true else (ErrorMsg.error 0 ("Different RECORD types."); false)
        | _ => (print("Inconsistent types\nOne is " ^ tostring(ty1) ^ ", the other is " ^ tostring(ty2) ^ "\n"); false)

  (* Check if ty2 is a subtype of ty1
     If ty2 is a subtype of ty1, return true; otherwise, false 
     Note that a type is a subtype of itself *)
  fun is_subtype_of(ty1, ty2) = 
      case (ty1, ty2) of
          (_, BOTTOM) => true
        | (UNIT, _) => true
        | (RECORD(_), NIL) => true
        | _ => are_the_same_type(ty1, ty2)
end
