structure Main = 
struct
  structure P = Parse
  structure S = Semant

  fun run filename = S.transProg(P.parse filename)
end
