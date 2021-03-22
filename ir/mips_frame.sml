structure MipsFrame : FRAME = 
struct

datatype access = InFrame of int
                | InReg of Temp.temp
type frame = {formals: access list, view_shift: Tree.stm list, numlocals: int ref, name: Temp.label}
datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string 
val FP = Temp.newtemp()
val RA = Temp.newtemp()
val argregs = [Temp.newtemp(), Temp.newtemp(), Temp.newtemp(), Temp.newtemp()] 

val wordSize = 4

fun newFrame {name=n, formals=fo} = 
    let val curr = ref 0 (* current number of locals *)
        val access_list =  map (fn (escaped) => if escaped 
                                                then (curr := !curr + 1; InFrame(!curr * (~4))) 
                                                else InReg(Temp.newtemp())) fo
                          
        val view_shift_insns =
	    let fun aux (al, argregs) = 
		    let val num_extra_args = ref 0
		    in
			case (al, argregs) of
                            ([], _) => []
                          | (a::al', reg::argregs') => Tree.MOVE(
							  (exp a (Tree.TEMP(FP))),
							  Tree.TEMP(reg)
						      )::aux(al', argregs')
                          (* (case a of
			     InFrame(offset) => Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST(offset))), Tree.TEMP(reg))::aux(al', argregs')
			     | InReg(tem) => Tree.MOVE(Tree.TEMP(tem), Tree.TEMP(reg))::aux(al', argregs')) *)
			  | (a::al', []) => (* has more arguments than argregs can hold *)
			    (
			      num_extra_args := !num_extra_args + 1;
			      Tree.MOVE(
                                  (exp a (Tree.TEMP(FP))), 
                                  Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST(4 * !num_extra_args)))
                              )::aux(al', [])
			      (* case a of
                                  InFrame(offset) => Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST(offset))),
							       (* Extra args reside in the previous frame, see page 127 *)
							       Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST(4 * !num_extra_args))))
						     ::aux(al', [])
                                | InReg(tem) => Tree.MOVE(Tree.TEMP(tem),
							  Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST(4 * !num_extra_args))))
						::aux(al', []) *)
			    )
		    end
            in
                aux (access_list, argregs)
            end
    in
        {
          formals = access_list,
          view_shift = view_shift_insns,
          numlocals = ref (!curr),
          name = n
        }
    end

and exp ac fp =
    case ac of
	InReg(tem) => Tree.TEMP(tem)
      | InFrame(k) => Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(k))) 
	
fun name(fr: frame) = 
    #name fr

fun formals(fr: frame) = 
    #formals fr
	     
fun allocLocal (fr: frame) escaped = 
    if escaped
    then ((#numlocals fr) := !(#numlocals fr) + 1; InFrame(!(#numlocals fr) * (~4)))
    else InReg(Temp.newtemp())

fun externalCall(s, args) =
    Tree.CALL(Tree.NAME(Temp.namedlabel s), args)

end
