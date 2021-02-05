structure Parse =
struct
  structure Lex = TigerLexFun(structure Tokens = Tokens) 
  fun parse filename =
      let val file1 = TextIO.openIn filename
          val file2 = TextIO.openIn filename
          fun gett _ = TextIO.input file2
          fun get _ = TextIO.input file1
	  val lexer = Mlex.makeLexer get
          val testlexer = Lex.makeLexer gett
	  fun do_it() =
	      let val t = lexer()
                  val tt = testlexer()
	       in if tt = t then () else print("Error: "^ t ^ "  |  " ^ tt ^ "\n");
		   if substring(tt,0,3)="EOF" then () else do_it()
	      end
       in do_it();
          TextIO.closeIn file1
      end

end

