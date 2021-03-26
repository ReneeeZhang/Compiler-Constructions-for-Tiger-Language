structure MipsFrame : FRAME = 
struct

structure TT = Temp.Table

datatype access = InFrame of int
                | InReg of Temp.temp
type frame = {formals: access list, view_shift: Tree.stm list, numlocals: int ref, name: Temp.label}
type register = string
datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string 
val FP = Temp.newtemp()
val RA = Temp.newtemp()
val argregs = [Temp.newtemp(), Temp.newtemp(), Temp.newtemp(), Temp.newtemp()] 

val tempMap = let val tmap1 = TT.enter(TT.empty, 100, "fp")
                  val tmap2 = TT.enter(tmap1, 101, "ra")
                  val tmap3 = TT.enter(tmap2, 102, "a0")
                  val tmap4 = TT.enter(tmap3, 103, "a1")
                  val tmap5 = TT.enter(tmap4, 104, "a2")
                  val tmap_final = TT.enter(tmap5, 105, "a3")
              in
                    tmap_final
              end

val wordSize = 4
val emptyFrame = {formals=([]: access list), view_shift=([]: Tree.stm list), numlocals=ref 0, name=Temp.newlabel()}
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
	
fun name {formals, view_shift, numlocals, name} = 
    name

fun formals {formals, view_shift, numlocals, name} = 
    formals

fun string (lab, s) = s (* TODO: need to rewrite later *)

fun display temp = 
    case TT.look(tempMap, temp) of
        SOME(regname) => regname
      | NONE => Temp.makestring(temp)
	     
fun allocLocal {formals, view_shift, numlocals, name} escaped = 
    if escaped
    then (numlocals := !numlocals + 1; InFrame(!numlocals * (~4)))
    else InReg(Temp.newtemp())

fun externalCall(s, args) =
    Tree.CALL(Tree.NAME(Temp.namedlabel s), args)

end
