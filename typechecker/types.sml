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

  (* TODO: should add subtype logic *)
  fun are_the_same_type(ty1, ty2) = (* ty * ty -> bool *)
      case (ty1, ty2) of
          (INT, INT) => true
        | (NIL, NIL) => true
        | (STRING, STRING) => true
        | (UNIT, UNIT) => true
        | (BOTTOM, BOTTOM) => true
        | (ARRAY(_, u1), ARRAY(_, u2)) => u1 = u2
        | (RECORD(_, u1), RECORD(_, u2)) => u1 = u2
        | _ => (print("Inconsistent types\n"); false)
end
