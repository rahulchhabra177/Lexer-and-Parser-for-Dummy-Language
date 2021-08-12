%%

%name boolean

%term(*Set of  terminals used are the foloowing as given in the assignment statement*)
  ID of string | AND | OR | XOR | NOT  | EQUALS | IMPLIES | IF | THEN | ELSE | RPAREN | LPAREN | EOF |TRUE |FALSE|TERM
(*Set of non terminals used are the foloowing as given in the assignment statement*)
%nonterm formula of string | program of string | statement of string|CONST of string

%pos int
%eop EOF
%noshift EOF
%start program
(*Right associative operators written in decreasing precedance*)
%right NOT IMPLIES IF THEN ELSE 

(*Left associative operators written in decreasing precedance*)
%left AND OR XOR EQUALS 

%%
(*Since the non terminnals are defined to be of type string I am storing them at each step and printing only if I reach the starting symbol of the grammar*)
program: statement(print ("]\n["^statement^"program:Statement]\n");statement)

statement: 	formula TERM(formula^" TERM\";\","^"statement:formula TERM,")|
			formula TERM statement(formula^"TERM\";\","^statement^"statement:formula TERM statement,")
formula: 	IF formula THEN formula ELSE formula("IF\"IF\","^formula1^"THEN\"THEN\","^formula2^"ELSE\"ELSE\","^formula3^" formula: IF formula THEN formula ELSE formula,") |
			NOT formula ("NOT \"NOT\","^formula^" formula :=> NOT formula,")|
			formula AND formula (formula1^" AND\"AND\","^formula2^" formula :=> formula AND formula,")|  
			formula OR formula (formula1^" OR\"OR\","^formula2^" formula :=> formula OR formula,")|  
			formula XOR formula (formula1^" XOR\"XOR\","^formula2^" formula :=> formula XOR formula,")|  
			formula EQUALS formula (formula1^" EQUALS\"EQUALS\","^formula2^" formula :=> formula EQUALS formula,")|  
			formula IMPLIES formula (formula1^" IMPLIES\"IMPLIES\","^formula2^" formula :=> formula IMPLIES formula,")|  
			LPAREN formula RPAREN ("LPAREN \"(\","^formula^"RPAREN\")\","^ " formula :=>LPAREN formula RPAREN,")|
			CONST(CONST^"statement: CONST,")|ID( "ID \""^(ID)^"\",")	

CONST: TRUE("CONST \"TRUE\",")|FALSE("CONST \"FALSE\",")

