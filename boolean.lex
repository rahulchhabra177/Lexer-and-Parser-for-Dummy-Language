structure Tokens= Tokens
  (*Standard declarations which are required in a usual .lex file*)
  type pos = int
  type col = int (*Used another variable col to take note of curent column number*)
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int,col:int,_) => TextIO.output(TextIO.stdOut,"]\nUnknown Token:" ^ (Int.toString l) ^ ":"^(Int.toString col)^":" ^ e ^ "\n\n") 
  val col=ref 1
 
%%
%header (functor CalculateLexFun(structure Tokens:boolean_TOKENS));

alpha=[A-Za-z];
whitespace = [\ \t];

%%

\n       				=> (pos := (!pos) + 1;col:= 1; lex());
{whitespace}+    		=> (col := (!col) + 1;lex());
"AND"       			=> (print "AND \"AND\", ";         col := (!col) + 3;  Tokens.AND(!pos,!col));
"OR"        			=> (print "OR \"OR\", ";             col := (!col) + 2;Tokens.OR(!pos,!col));
"("         			=> (print "LPAREN \"(\", ";          col := (!col) + 1;Tokens.LPAREN(!pos,!col));
")"        				=> (print "RPAREN \")\", ";          col := (!col) + 1;Tokens.RPAREN(!pos,!col));
"XOR"       			=> (print "XOR \"XOR\", ";          col := (!col) + 3; Tokens.XOR(!pos,!col));
"TRUE"      			=> (print "CONST \"TRUE\", ";         col := (!col) + 4;Tokens.TRUE(!pos,!col));
"FALSE"     			=> (print "CONST \"FALSE\", ";      col := (!col) + 5; Tokens.FALSE(!pos,!col));
"EQUALS"    			=> (print "EQUALS \"EQUALS\", ";    col := (!col) + 6; Tokens.EQUALS(!pos,!col));
"IMPLIES"    			=> (print "IMPLIES \"IMPLIES\", ";  col := (!col) + 7; Tokens.IMPLIES(!pos,!col));
"IF"        			=> (print "IF \"IF\", ";            col := (!col) + 2; Tokens.IF(!pos,!col));
"THEN"      			=> (print "THEN \"THEN\", ";        col := (!col) + 4; Tokens.THEN(!pos,!col));
"ELSE"      			=> (print "ELSE \"ELSE\", ";        col := (!col) + 4; Tokens.ELSE(!pos,!col));
";"         			=> (print "TERM \";\" ";      	col := (!col) + 1;	Tokens.TERM(!pos,!col));
"NOT"       			=> (print "NOT \"NOT\", ";          col := (!col) + 3; Tokens.NOT(!pos,!col));
{alpha}{alpha}*   	=> (print "ID \"";(print yytext);print "\", ";col := (!col) + size(yytext);Tokens.ID(yytext,!pos,!col));
.         				=> (error (yytext,!pos,!col,!pos);lex());
