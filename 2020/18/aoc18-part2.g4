grammar Exp2;

expression
	: binaryExp 
	| numberExp
	| parensExp ;

binaryExp 
	: mulExp 
	| addExp ;

mulExp 
	: mulOperandExp (MUL mulOperandExp)+ ;

addExp 
	: addOperandExp (ADD addOperandExp)+ ;

mulOperandExp
	: addExp 
	| numberExp 
	| parensExp ;

addOperandExp 
	: numberExp 
	| parensExp ;

parensExp 
	: LPAREN binaryExp RPAREN ;

numberExp : INT ;

INT : [0-9]+ ;

LPAREN : '(' ;

RPAREN : ')' ;

ADD : '+' ;

MUL : '*' ;