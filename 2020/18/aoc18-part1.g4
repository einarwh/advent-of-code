grammar Exp1;

expression
	: expression (ADD | MUL)  expression
	| LPAREN expression RPAREN
	| INT
	;

INT : [0-9]+ ;

LPAREN : '(' ;

RPAREN : ')' ;

ADD : '+' ;

MUL : '*' ;