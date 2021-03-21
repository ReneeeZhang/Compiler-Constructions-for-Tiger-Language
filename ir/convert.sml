structure ConvertFor = 
struct 
fun get_absyn(lo, hi, body,pos, escape) = 
  Absyn.LetExp{decs=
        [Absyn.VarDec{name=Symbol.symbol("i"), escape=escape,typ=NONE, init=lo,
         pos=pos},
        Absyn.VarDec{name=Symbol.symbol("limit"), escape=escape, typ=NONE,
        init=hi, pos=pos}],
   
   body=Absyn.WhileExp{test=Absyn.OpExp{left=Absyn.VarExp(Absyn.SimpleVar(Symbol.symbol("i"),
   pos)), oper=Absyn.LeOp,
   right=Absyn.VarExp(Absyn.SimpleVar(Symbol.symbol("limit"),pos)),pos=pos},
   body=Absyn.SeqExp[(body,pos), 
   
   (Absyn.AssignExp{var=Absyn.SimpleVar(Symbol.symbol("i"), pos),
    exp=Absyn.OpExp{left=Absyn.VarExp(Absyn.SimpleVar(Symbol.symbol("i"), pos)),
    oper=Absyn.PlusOp, right=Absyn.IntExp(1),pos=pos}, pos=pos},pos)], pos=pos},
  
   pos=pos} 
end
