type pos = int
type lexresult = Tokens.token


val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1
val strcnt = ref 0;
fun eof() = 
let 
  val pos = hd(!linePos) 
  val u = if !strcnt = 0 then () else ErrorMsg.error pos ("open string") 
in 
  Tokens.EOF(pos,pos) 
end


val strbuf = ref "";

%%
%s STR ESC;
dig = [0-9];
alpha = [A-Za-z];
asciicodes = [0][0-9][0-9]|[1][0-1][0-9]|[1][2][0-7];

%%
<INITIAL>\n             => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>var            => (Tokens.VAR(yypos,yypos+3));
<INITIAL>","            => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>"123"          => (Tokens.INT(123,yypos,yypos+3));


<INITIAL>["]            => (YYBEGIN STR; strbuf := ""; strcnt := !strcnt + 1; continue());
<STR>["]                => (YYBEGIN INITIAL; strcnt := !strcnt - 1; Tokens.STRING((!strbuf), yypos, yypos + size (!strbuf)));
<STR>[^"\\]             => (strbuf := !strbuf ^ yytext; continue());
<STR>[\\]               => (YYBEGIN ESC; strbuf := !strbuf ^ yytext; continue());
<STR>[\\][\n\t ]+[\\]   => (continue());
<ESC>"^"[a-z]           => (YYBEGIN STR; strbuf := !strbuf ^ yytext; continue());
<ESC>[nt\\"]		=> (YYBEGIN STR; strbuf := !strbuf ^ yytext; continue());
<ESC>{asciicodes}	=> (YYBEGIN STR; strbuf := !strbuf ^ yytext; continue());
<ESC>.        		=> (ErrorMsg.error yypos ("illegal escape"); YYBEGIN STR; strbuf := !strbuf ^ yytext; continue());


<INITIAL>.              => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());



