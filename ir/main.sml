structure Main = 
struct
  structure P = Parse
  structure S = Semant
  structure F = FindEscape
  structure PT = Printtree
  structure T = Tree

  fun print_treelist ([]) = ()
    | print_treelist (h::t) = (PT.printtree(TextIO.stdOut, h); print_treelist t)

  fun frags ([]) = print("End \n")
    | frags (MipsFrame.PROC{body=body, frame=frame}::t) =
      let
        val name = #name frame
      in
        (print(Symbol.name(name) ^ " : \n"); print_treelist(Canon.linearize(body)); frags(t))
      end
    | frags (MipsFrame.STRING(l, s)::t) = (print(Symbol.name(l) ^ " : " ^ s ^ 
    " \n"); frags(t))
(*
  fun esc filename = 
    let 
      val ast = P.parse filename
    in
      (F.findEscape ast; PrintAbsyn.print(TextIO.stdOut, ast))
    end
  fun run filename = 
    let
      val ast = P.parse filename
    in
      (F.findEscape ast; S.transProg(ast))
    end
  fun print filename = 
    let 
      val ast = P.parse filename
      val checked = (F.findEscape ast; S.transProg ast)
    in
      PT.printtree(TextIO.stdOut, T.EXP(Translate.unEx(#exp checked)))
    end
 *)
  fun lin filename = 
    let
      val ast = P.parse filename
      val f = (F.findEscape ast; S.transProg ast)
    in
      frags(f)
    end
end
