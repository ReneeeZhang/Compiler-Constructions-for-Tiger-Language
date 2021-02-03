type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1
val strcnt = ref 0;
val strbuf = ref "";

val comment_depth = ref 0

fun eof() = 
let 
  val pos = hd(!linePos) 
in 
  if !strcnt = 0 then () else ErrorMsg.error pos ("Open string");
  if !comment_depth = 0 then () else ErrorMsg.error pos ("Unmatched comment tags");
  Tokens.EOF(pos,pos) 
end

%%
%s STR ESC COMMENT;
dig = [0-9];
alpha = [A-Za-z];
asciicodes = [0][0-9][0-9]|[1][0-1][0-9]|[1][2][0-7];
%%
<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>"," => (Tokens.COMMA(yypos, yypos + 1));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos + 1));
<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos + 1));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos + 1));
<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos + 1));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos + 1));
<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos + 1));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos + 1));
<INITIAL>"." => (Tokens.DOT(yypos, yypos + 1));
<INITIAL>"+" => (Tokens.PLUS(yypos, yypos + 1));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos + 1));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos + 1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos + 1));
<INITIAL>"=" => (Tokens.EQ(yypos, yypos + 1));
<INITIAL>"<>" => (Tokens.NEQ(yypos, yypos + 2));
<INITIAL>"<" => (Tokens.LT(yypos, yypos + 1));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos + 2));
<INITIAL>">" => (Tokens.GT(yypos, yypos + 1));
<INITIAL>">=" => (Tokens.GE(yypos, yypos + 2));
<INITIAL>"&" => (Tokens.AND(yypos, yypos + 1));
<INITIAL>"|" => (Tokens.OR(yypos, yypos + 1));
<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos + 2));
<INITIAL>":" => (Tokens.COLON(yypos, yypos + 1));
<INITIAL>while => (Tokens.WHILE(yypos, yypos + 5));
<INITIAL>for => (Tokens.FOR(yypos, yypos + 3));
<INITIAL>to => (Tokens.TO(yypos, yypos + 2));
<INITIAL>break => (Tokens.BREAK(yypos, yypos + 5));
<INITIAL>let => (Tokens.LET(yypos, yypos + 3));
<INITIAL>in => (Tokens.IN(yypos, yypos + 2));
<INITIAL>end => (Tokens.END(yypos, yypos + 3));
<INITIAL>function => (Tokens.FUNCTION(yypos, yypos + 8));
<INITIAL>var => (Tokens.VAR(yypos, yypos + 3));
<INITIAL>type => (Tokens.TYPE(yypos, yypos + 4));
<INITIAL>array => (Tokens.ARRAY(yypos, yypos + 5));
<INITIAL>if => (Tokens.IF(yypos, yypos + 2));
<INITIAL>then => (Tokens.THEN(yypos, yypos + 4));
<INITIAL>else => (Tokens.ELSE(yypos, yypos + 4));
<INITIAL>do => (Tokens.DO(yypos, yypos + 2));
<INITIAL>of => (Tokens.OF(yypos, yypos + 2));
<INITIAL>nil => (Tokens.NIL(yypos, yypos + 3));
<INITIAL>[a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL>[0-9]+	=> (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos + size yytext));
<INITIAL>["]   => (YYBEGIN STR; strbuf := ""; strcnt := !strcnt + 1; continue());
<STR>["]       => (YYBEGIN INITIAL; strcnt := !strcnt - 1; Tokens.STRING((!strbuf), yypos, yypos + size (!strbuf)));
<STR>[^"\\]    => (strbuf := !strbuf ^ yytext; continue());
<STR>[\\]     => (YYBEGIN ESC; strbuf := !strbuf ^ yytext; continue());
<STR>[\\][\n\t ]+[\\]   => (continue());
<ESC>"^"[a-z]     => (YYBEGIN STR; strbuf := !strbuf ^ yytext; continue());
<ESC>[nt\\"]   => (YYBEGIN STR; strbuf := !strbuf ^ yytext; continue());
<ESC>{asciicodes}      => (YYBEGIN STR; strbuf := !strbuf ^ yytext; continue());
<ESC>.        => (ErrorMsg.error yypos ("illegal escape"); YYBEGIN STR; strbuf := !strbuf ^ yytext; continue());

<INITIAL>(" "|"\t")+ => (continue());
<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<INITIAL>"/*"   => (YYBEGIN COMMENT; comment_depth := !comment_depth + 1; continue());
<COMMENT>"/*"   => (comment_depth := !comment_depth + 1; continue());
<COMMENT>"*/"   => (comment_depth := !comment_depth - 1;
                    if !comment_depth = 0 then YYBEGIN INITIAL else YYBEGIN COMMENT;
                    continue());

<COMMENT>\n	    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT>.      => (continue());
