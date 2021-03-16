structure MipsFrame : FRAME = 
struct
    (* Define this based on page 136 *)
    (* TODO: view shift *)
    datatype access = InFrame of int
                    | InReg of Temp.temp
    type frame = {formals: access list, view_shift: unit, numlocals: int, name: Temp.label, sp: int ref}

    fun newFrame {name=n, formals=fo} = 
        let val curr = ref 0
        in
            {
                formals = map 
                        (fn (escaped) => if escaped 
                                         then (curr := !curr - 4; InFrame(!curr)) 
                                         else InReg(Temp.newtemp())) 
                        fo,
                view_shift = (),
                numlocals = 0,
                name = n,
                sp = curr
            }
        end

    fun name(fr: frame) = 
        #name fr

    fun formals(fr: frame) = 
        #formals fr
    
    fun allocLocal (fr: frame) escaped = 
        if escaped
        then ((#sp fr) := !(#sp fr) - 4; InFrame(!(#sp fr)))
        else InReg(Temp.newtemp())
end
