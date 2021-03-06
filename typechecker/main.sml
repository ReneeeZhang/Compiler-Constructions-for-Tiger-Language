structure Main = 
struct
  structure P = Parse
  structure S = Semant
  structure F = FindEscape

  fun esc filename = 
    let 
      val ast = P.parse filename
    in
      (F.findEscape ast; PrintAbsyn.print(TextIO.stdOut, ast))
    end
  fun run filename = S.transProg(P.parse filename)
end
