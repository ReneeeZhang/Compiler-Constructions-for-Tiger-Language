signature FRAME =
sig
    type frame (* The type frame holds information about formal parameters and local variables allocated in this frame *)
    datatype access = InFrame of int
                    | InReg of Temp.temp(* The access type describes formals and locals that may be in the frame or in registers *)
    val newFrame : {name: Temp.label, formals: bool list} -> frame (* a true in the formals indicates an escaped formal parameter *)
    val name : frame -> Temp.label (* get the name of a frame *)
    val formals : frame -> access list (* denote the locations where the formal parameters will be kept at run time, as seen from inside the callee. *)
    val allocLocal : frame -> bool -> access (* allocate a new local variable in a frame; The boolean argument to allocLocal 
                                                specifies whether the new variable escapes and needs to go in the frame; 
                                                if it is false, then the variable can be allocated in a register. *)  
    val argregs : Temp.temp list
    val FP : Temp.temp
    val RA : Temp.temp
    val wordSize : int
    val exp : access -> Tree.exp -> Tree.exp (* used by Translate to turn a Frame.access into the Tree expression. 
                                            The Tree.exp argument to Frame.exp is the address of the stack frame that the access lives in *)
    val externalCall: string * Tree.exp list -> Tree.exp
    (* Uncomment as needed - for advanced functionalities *)
    (* 
    val RV : Temp. temp
	*)
    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string 

end
