structure MipsFrame : FRAME = 
struct

structure TT = Temp.Map

datatype access = InFrame of int
                | InReg of Temp.temp
type frame = {formals: access list, view_shift: Tree.stm list, numlocals: int ref, name: Temp.label}
type register = string
datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string 
structure RegSet = SplaySetFn(struct 
                               type ord_key = register
                               val compare = String.compare
                               end)

(* Do NOT change the order of the following newtemp declarations *)
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

val tempMap = let val tmap1 = TT.insert(TT.empty, FP, "$fp")
                  val tmap2 = TT.insert(tmap1, RA, "$ra")
                  (* argregs *)
                  val tmap3 = TT.insert(tmap2, List.nth(argregs, 0), "$a0")
                  val tmap4 = TT.insert(tmap3, List.nth(argregs, 1), "$a1")
                  val tmap5 = TT.insert(tmap4, List.nth(argregs, 2), "$a2")
                  val tmap6 = TT.insert(tmap5, List.nth(argregs, 3), "$a3")
                  (* rvs *)
                  val tmap7 = TT.insert(tmap6, List.nth(RVs, 0), "$v0")
                  val tmap8 = TT.insert(tmap7, List.nth(RVs, 1), "$v1")
                  (* callersaves *)
                  val tmap9 = TT.insert(tmap8, List.nth(callersaves, 0), "$t0")
                  val tmap10 = TT.insert(tmap9, List.nth(callersaves, 1), "$t1")
                  val tmap11 = TT.insert(tmap10, List.nth(callersaves, 2), "$t2")
                  val tmap12 = TT.insert(tmap11, List.nth(callersaves, 3), "$t3")
                  val tmap13 = TT.insert(tmap12, List.nth(callersaves, 4), "$t4")
                  val tmap14 = TT.insert(tmap13, List.nth(callersaves, 5), "$t5")
                  val tmap15 = TT.insert(tmap14, List.nth(callersaves, 6), "$t6")
                  val tmap16 = TT.insert(tmap15, List.nth(callersaves, 7), "$t7")
                  (* calleesaves *)
                  val tmap17 = TT.insert(tmap16, List.nth(calleesaves, 0), "$s0")
                  val tmap18 = TT.insert(tmap17, List.nth(calleesaves, 1), "$s1")
                  val tmap19 = TT.insert(tmap18, List.nth(calleesaves, 2), "$s2")
                  val tmap20 = TT.insert(tmap19, List.nth(calleesaves, 3), "$s3")
                  val tmap21 = TT.insert(tmap20, List.nth(calleesaves, 4), "$s4")
                  val tmap22 = TT.insert(tmap21, List.nth(calleesaves, 5), "$s5")
                  val tmap23 = TT.insert(tmap22, List.nth(calleesaves, 6), "$s6")
                  val tmap24 = TT.insert(tmap23, List.nth(calleesaves, 7), "$s7")
                  (* special regs *)
                  val tmap25 = TT.insert(tmap24, List.nth(specialregs, 0), "$zero")
                  val tmap26 = TT.insert(tmap25, List.nth(specialregs, 1), "$at")
                  val tmap27 = TT.insert(tmap26, List.nth(specialregs, 2), "$sp")
                  val tmap28 = TT.insert(tmap27, List.nth(specialregs, 3), "$gp")
                  val tmap29 = TT.insert(tmap28, List.nth(specialregs, 4), "$t8")
                  val tmap30 = TT.insert(tmap29, List.nth(specialregs, 5), "$t9")

                  val tmap_final = tmap30
              in
                    tmap_final
              end

val availableRegs = let val ar1 = RegSet.add(RegSet.empty, "$v0")
                        val ar2 = RegSet.add(ar1, "$v1")
                        val ar3 = RegSet.add(ar2, "$a0")
                        val ar4 = RegSet.add(ar3, "$a1")
                        val ar5 = RegSet.add(ar4, "$a2")
                        val ar6 = RegSet.add(ar5, "$a3")
                        val ar7 = RegSet.add(ar6, "$t0")
                        val ar8 = RegSet.add(ar7, "$t1")
                        val ar9 = RegSet.add(ar8, "$t2")
                        val ar10 = RegSet.add(ar9, "$t3")
                        val ar11 = RegSet.add(ar10, "$t4")
                        val ar12 = RegSet.add(ar11, "$t5")
                        val ar13 = RegSet.add(ar12, "$t6")
                        val ar14 = RegSet.add(ar13, "$t7")
                        val ar15 = RegSet.add(ar14, "$t8")
                        val ar16 = RegSet.add(ar15, "$t9")
                        val ar17 = RegSet.add(ar16, "$s0")
                        val ar18 = RegSet.add(ar17, "$s1")
                        val ar19 = RegSet.add(ar18, "$s2")
                        val ar20 = RegSet.add(ar19, "$s3")
                        val ar21 = RegSet.add(ar20, "$s4")
                        val ar22 = RegSet.add(ar21, "$s5")
                        val ar23 = RegSet.add(ar22, "$s6")
                        val ar24 = RegSet.add(ar23, "$s7")
                        val ar25 = RegSet.add(ar24, "$sp")
                        val ar26 = RegSet.add(ar25, "$fp")
                        val ar27 = RegSet.add(ar26, "$ra")

                        val arfinal = ar27
                    in
                        arfinal
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

fun string (lab, s) = (".data\n" ^ Symbol.name(lab) ^ ": \n.word " ^ Int.toString(size(s)) ^ "\n.ascii \"" ^ s ^ "\"\n.text\n")

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
