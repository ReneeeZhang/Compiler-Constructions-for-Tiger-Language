signature FRAME =
sig
    val FP : Temp.temp 
    val wordSize: int 
    val exp : access -> Tree.exp -> Tree.exp
    val externalCall: string * Tree.exp list -> Tree.exp
    val RV : Temp. temp
    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

end