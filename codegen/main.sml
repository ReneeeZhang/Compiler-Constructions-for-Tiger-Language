structure Main = struct

structure Tr = Translate
structure MF = MipsFrame
(* structure R = RegAlloc *)

fun getsome (SOME x) = x

fun emitproc out (MF.PROC{body,frame}) =
    let val _ = print ("emit " ^ Symbol.name(MF.name frame) ^ "\n")
	(*         val _ = Printtree.printtree(out,body); *)
	val stms = Canon.linearize body
	(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	val instrs =   List.concat(map (MipsGen.codegen frame) stms') 
        val format0 = Assem.format(Temp.makestring)
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
