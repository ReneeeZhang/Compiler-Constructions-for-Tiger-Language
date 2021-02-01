type pos = int
type lexresult = Tokens.token


val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
val strcnt = ref 0;
val strbuf = ref "";

%%
%s STR ESC;
dig = [0-9];
alpha = [A-Za-z]; 
%%
<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>","     => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"123"	=> (Tokens.INT(123,yypos,yypos+3));
<INITIAL>["]   => (YYBEGIN STR; strbuf := ""; strcnt := !strcnt + 1; continue());
<STR>["]       => (YYBEGIN INITIAL; strcnt := !strcnt - 1; Tokens.STRING((!strbuf), yypos, yypos + size (!strbuf)));
<STR>[^"]    => (strbuf := !strbuf ^ yytext; continue());




<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());



