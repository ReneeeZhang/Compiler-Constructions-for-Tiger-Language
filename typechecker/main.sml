structure Main = 
struct
  structure P = Parse
  structure S = Semant
  structure F = FindEscape

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
      (F.findEscape ast; S.transProg ast)
    end
end
