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

  fun are_the_same_type(ty1, ty2, pos) = (* ty * ty -> bool *)
      case (ty1, ty2) of
          (INT, INT) => true
        | (NIL, NIL) => true 
        | (STRING, STRING) => true
        | (UNIT, UNIT) => true
        | (BOTTOM, BOTTOM) => true
        | (ARRAY(_, u1), ARRAY(_, u2)) => u1 = u2
        | (RECORD(_, u1), RECORD(_, u2)) => if u1 = u2 then true else
          (ErrorMsg.error pos ("Different RECORD types."); false)
        | _ => (ErrorMsg.error pos ("Type mismatch error: " ^ tostring(ty1) ^ " and " ^ tostring(ty2)); false)

  (* Check if ty2 is a subtype of ty1
     If ty2 is a subtype of ty1, return true; otherwise, false 
     Note that a type is a subtype of itself *)
  fun is_subtype_of(ty1, ty2, pos) = 
      case (ty1, ty2) of
          (_, BOTTOM) => true
        | (BOTTOM, _) => true
        | (UNIT, _) => true
        | (RECORD(_), NIL) => true
        | (NIL, RECORD(_)) => true
        | _ => are_the_same_type(ty1, ty2, pos)
end
