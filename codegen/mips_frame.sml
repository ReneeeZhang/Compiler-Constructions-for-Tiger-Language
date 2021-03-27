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
val RVs = [Temp.newtemp(), Temp.newtemp()]
val callersaves = [Temp.newtemp(), Temp.newtemp(), Temp.newtemp(), Temp.newtemp(),
                   Temp.newtemp(), Temp.newtemp(), Temp.newtemp(), Temp.newtemp()]
val calleesaves = [Temp.newtemp(), Temp.newtemp(), Temp.newtemp(), Temp.newtemp(),
                   Temp.newtemp(), Temp.newtemp(), Temp.newtemp(), Temp.newtemp()]
val ZERO = Temp.newtemp()
val AT = Temp.newtemp()
val SP = Temp.newtemp()
val GP = Temp.newtemp()
val extratemp1 = Temp.newtemp() (* No.24 in MIPS *)
val extratemp2 = Temp.newtemp() (* No.25 in MIPS *)
val specialregs = [ZERO, AT, SP, GP, extratemp1, extratemp2] (* Add special regs as needed *)                                                     

val tempMap = let val tmap1 = TT.enter(TT.empty, 100, "$fp")
                  val tmap2 = TT.enter(tmap1, 101, "$ra")
                  (* argregs *)
                  val tmap3 = TT.enter(tmap2, 102, "$a0")
                  val tmap4 = TT.enter(tmap3, 103, "$a1")
                  val tmap5 = TT.enter(tmap4, 104, "$a2")
                  val tmap6 = TT.enter(tmap5, 105, "$a3")
                  (* rvs *)
                  val tmap7 = TT.enter(tmap6, 106, "$v0")
                  val tmap8 = TT.enter(tmap7, 107, "$v1")
                  (* callersaves *)
                  val tmap9 = TT.enter(tmap8, 108, "$t0")
                  val tmap10 = TT.enter(tmap9, 109, "$t1")
                  val tmap11 = TT.enter(tmap10, 110, "$t2")
                  val tmap12 = TT.enter(tmap11, 111, "$t3")
                  val tmap13 = TT.enter(tmap12, 112, "$t4")
                  val tmap14 = TT.enter(tmap13, 113, "$t5")
                  val tmap15 = TT.enter(tmap14, 114, "$t6")
                  val tmap16 = TT.enter(tmap15, 115, "$t7")
                  (* calleesaves *)
                  val tmap17 = TT.enter(tmap16, 116, "$s0")
                  val tmap18 = TT.enter(tmap17, 117, "$s1")
                  val tmap19 = TT.enter(tmap18, 118, "$s2")
                  val tmap20 = TT.enter(tmap19, 119, "$s3")
                  val tmap21 = TT.enter(tmap20, 120, "$s4")
                  val tmap22 = TT.enter(tmap21, 121, "$s5")
                  val tmap23 = TT.enter(tmap22, 122, "$s6")
                  val tmap24 = TT.enter(tmap23, 123, "$s7")
                  (* special regs *)
                  val tmap25 = TT.enter(tmap24, 124, "$zero")
                  val tmap26 = TT.enter(tmap25, 125, "$at")
                  val tmap27 = TT.enter(tmap26, 126, "$zero")
                  val tmap28 = TT.enter(tmap27, 127, "$sp")
                  val tmap29 = TT.enter(tmap28, 128, "$gp")
                  val tmap30 = TT.enter(tmap29, 129, "$t8")
                  val tmap31 = TT.enter(tmap30, 130, "$t9")

                  val tmap_final = tmap31
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

fun procEntryExit1(frame, body) = body (* Need updating later *)

fun procEntryExit2(frame, body) = (* Need updating later *)
    body @ [Assem.OPER{assem="", src =[ZERO,RA,SP]@calleesaves, dst=[], jump=SOME[]}]

fun procEntryExit3({formals, view_shift, numlocals, name}, body) = (* Need updating later *)
    {prolog = "PROCEDURE " ^ Symbol.name name ^ "\n", body = body, epilog = "END " ^ Symbol.name name ^ "\n"}

end
