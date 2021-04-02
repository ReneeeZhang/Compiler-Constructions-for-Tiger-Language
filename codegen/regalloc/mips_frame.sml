structure MipsFrame : FRAME = 
struct

structure TT = Temp.Map

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

val tempMap = let val tmap1 = TT.insert(TT.empty, 100, "$fp")
                  val tmap2 = TT.insert(tmap1, 101, "$ra")
                  (* argregs *)
                  val tmap3 = TT.insert(tmap2, 102, "$a0")
                  val tmap4 = TT.insert(tmap3, 103, "$a1")
                  val tmap5 = TT.insert(tmap4, 104, "$a2")
                  val tmap6 = TT.insert(tmap5, 105, "$a3")
                  (* rvs *)
                  val tmap7 = TT.insert(tmap6, 106, "$v0")
                  val tmap8 = TT.insert(tmap7, 107, "$v1")
                  (* callersaves *)
                  val tmap9 = TT.insert(tmap8, 108, "$t0")
                  val tmap10 = TT.insert(tmap9, 109, "$t1")
                  val tmap11 = TT.insert(tmap10, 110, "$t2")
                  val tmap12 = TT.insert(tmap11, 111, "$t3")
                  val tmap13 = TT.insert(tmap12, 112, "$t4")
                  val tmap14 = TT.insert(tmap13, 113, "$t5")
                  val tmap15 = TT.insert(tmap14, 114, "$t6")
                  val tmap16 = TT.insert(tmap15, 115, "$t7")
                  (* calleesaves *)
                  val tmap17 = TT.insert(tmap16, 116, "$s0")
                  val tmap18 = TT.insert(tmap17, 117, "$s1")
                  val tmap19 = TT.insert(tmap18, 118, "$s2")
                  val tmap20 = TT.insert(tmap19, 119, "$s3")
                  val tmap21 = TT.insert(tmap20, 120, "$s4")
                  val tmap22 = TT.insert(tmap21, 121, "$s5")
                  val tmap23 = TT.insert(tmap22, 122, "$s6")
                  val tmap24 = TT.insert(tmap23, 123, "$s7")
                  (* special regs *)
                  val tmap25 = TT.insert(tmap24, 124, "$zero")
                  val tmap26 = TT.insert(tmap25, 125, "$at")
                  val tmap27 = TT.insert(tmap26, 126, "$sp")
                  val tmap28 = TT.insert(tmap27, 127, "$gp")
                  val tmap29 = TT.insert(tmap28, 128, "$t8")
                  val tmap30 = TT.insert(tmap29, 129, "$t9")

                  val tmap_final = tmap30
              in
                    tmap_final
              end

val wordSize = 4
val emptyFrame = {formals=([]: access list), view_shift=([]: Tree.stm list), numlocals=ref 0, name=Temp.newlabel()}
fun newFrame {name=n, formals=fo} = 
    let val curr = ref 0 (* current number of locals *)
        val access_list =
	    let val eff_formals = case fo of
				      [] => []
				    | _::restfo => restfo
	    in
		map (fn (escaped) => if escaped 
                                     then (curr := !curr + 1; InFrame(!curr * (~4))) 
                                     else InReg(Temp.newtemp())) eff_formals
            end
		
        val view_shift_insns =
	    let fun aux (al, argregs, num_extra_args) = 
		    case (al, argregs) of
                        ([], _) => []
                      | (a::al', reg::argregs') =>
			Tree.MOVE(
			    (exp a (Tree.TEMP(FP))),
			    Tree.TEMP(reg)
			)::aux(al', argregs', num_extra_args)
		      | (a::al', []) => (* has more arguments than argregs can hold *)
			Tree.MOVE(
			    (exp a (Tree.TEMP(FP))), 
			    Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST(4 * (num_extra_args + 1))))
			)::aux(al', [], num_extra_args + 1)    
            in
                Tree.LABEL(n)::aux(access_list, argregs, 0)
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

fun string (lab, s) = (Symbol.name(lab) ^ ": .asciiz \"" ^ s ^ "\"\n")

fun display temp = 
    case TT.find(tempMap, temp) of
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
