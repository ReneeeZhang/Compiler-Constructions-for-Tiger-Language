structure Main = struct

structure Tr = Translate
structure MF = MipsFrame
structure L = Liveness
(* structure R = RegAlloc *)

fun getsome (SOME x) = x

fun emitproc out (MF.PROC{body,frame}) =
    let val _ = print ("emit " ^ Symbol.name(MF.name frame) ^ "\n")
	(*         val _ = Printtree.printtree(out,body); *)
	val stms = Canon.linearize body
	(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	    val instrs =   List.concat(map (MipsGen.codegen frame) stms') 
        (* CFG *)
        val (cfg as {control, def, use, ismove}) = MakeGraph.instrs2graph(instrs)
        (* val _ = print("------------------- CFG ------------------\n")
        val _ = Flow.printControlGraph control
        val _ = print("------------------- DEF & USE ----------------------\n")
        val _ = Flow.printLabelMap(def, "DEF")
        val _ = print("**********************************************\n")
        val _ = Flow.printLabelMap(use, "USE") *)
	    val _ = print("==============================================\n")
        (* Liveness Info *)
        val liveness = L.calculateLiveness(control, def, use)
	    val _ = Flow.printLabelMap(liveness, "LIVENESS")
	    val _ = print("-------------- Interference Graph ---------------\n")
        val {graph, moves} = L.generateIGraph(cfg)
        val _ = L.printIGraph(graph)
            (* val _
			 = map (fn (x) => print(case x of
					     Assem.LABEL({assem, lab}) => "label " ^ assem
					   | Assem.MOVE({assem, dst, src}) => assem
					   | Assem.OPER({assem, dst, src, jump}) => assem)) instrs *)
        val format0 = Assem.format(MF.display)
    in  app (fn i => TextIO.output(out,format0 i)) instrs
    end
  | emitproc out (MF.STRING(lab,s)) = TextIO.output(out,MF.string(lab,s))


fun withOpenFile fname f = 
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out) 
       handle e => (TextIO.closeOut out; raise e)
    end 

fun compile filename = 
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
    in 
        withOpenFile (filename ^ ".s") 
		     (fn out => (app (emitproc out) frags))
    end

end
