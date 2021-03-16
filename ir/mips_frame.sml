structure MipsFrame : FRAME = 
struct
    (* Define this based on page 136 *)
    (* TODO: view shift *)
    type frame = {formals: access list, view_shift: unit, numlocals: int, name: Temp.label, sp: ref int}
    datatype access = InFrame of int
                    | InReg of Temp.temp

    fun newFrame {name=n, formals=fo} = 
        let val curr = ref 0
        in
            {
                formals = map 
                        (fn (escaped) => if escaped 
                                         then (curr := curr - 4; InFrame(!curr)) 
                                         else InReg(Temp.newtemp())) 
                        fo,
                view_shift = unit,
                numlocals = 0,
                name = n,
                sp := !curr
            }
        end

    fun name(fr: frame) = 
        #name fr

    fun formals(fr: frame) = 
        #formals fr
    
    fun allocLocal fr escaped = 
        if escaped
        then (fr.sp := fr.sp - 4; InFrame(!fr.sp))
        else InReg(Temp.newtemp())
end