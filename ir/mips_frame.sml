structure MipsFrame : FRAME = 
struct
    (* Define this based on page 136 *)
    (* TODO: view shift *)
    datatype access = InFrame of int
                    | InReg of Temp.temp
    type frame = {formals: access list, view_shift: unit, numlocals: int ref, name: Temp.label}

    fun newFrame {name=n, formals=fo} = 
        let val curr = ref 0 (* current number of locals *)
        in
            {
                formals = map 
                        (fn (escaped) => if escaped 
                                         then (curr := !curr + 1; InFrame(!curr * (~4))) 
                                         else InReg(Temp.newtemp())) 
                        fo,
                view_shift = (),
                numlocals = ref (!curr),
                name = n
            }
        end

    fun name(fr: frame) = 
        #name fr

    fun formals(fr: frame) = 
        #formals fr
    
    fun allocLocal (fr: frame) escaped = 
        if escaped
        then ((#numlocals fr) := !(#numlocals fr) + 1; InFrame(!(#numlocals fr) * (~4)))
        else InReg(Temp.newtemp())
end
