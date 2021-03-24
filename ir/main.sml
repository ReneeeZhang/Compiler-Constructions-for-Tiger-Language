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

  fun frags_full ([]) = print("End \n")
    | frags_full (MipsFrame.PROC{body=body, frame=frame}::t) =
      let
        val name = #name frame
      in
        (print(Symbol.name(name) ^ " : \n"); PT.printtree(TextIO.stdOut,body);
         frags_full(t))
      end
    | frags_full (MipsFrame.STRING(l, s)::t) = (print(Symbol.name(l) ^ " : " ^ s ^ 
    " \n"); frags_full(t))


  fun run filename = 
    let
      val ast = P.parse filename
      val f = (F.findEscape ast; S.transProg ast)
    in
      frags_full(f)
    end

  fun lin filename = 
    let
      val ast = P.parse filename
      val f = (F.findEscape ast; S.transProg ast)
    in
      frags(f)
    end
end
