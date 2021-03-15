structure Main = 
struct
  structure P = Parse
  structure S = Semant
  structure F = FindEscape
  structure PT = Printtree
  structure T = Tree

  fun esc filename = 
    let 
      val ast = P.parse filename
    in
      (F.findEscape ast)
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
      (F.findEscape ast; PT.printtree(TextIO.stdOut, T.EXP(Translate.unEx(#exp
      checked))))
    end
end
