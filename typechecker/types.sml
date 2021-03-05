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
end
