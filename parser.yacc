%%
%name A3
%term
	EOF | TERM | IF | THEN | ELSE | FI | IMPLIES | NOT | LPAREN | RPAREN | AND | OR | XOR | EQUALS | PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN | LET | ASSIGN | IN | END | BOOL of string | NUM of int | ID of string
%nonterm
	InputFile of AST.exp list | program of AST.exp list | statement of AST.exp | formula of AST.exp
%pos int

%start InputFile

%eop EOF
%noshift EOF

%right IF THEN ELSE FI
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%left NEGATE

%verbose

%%
	InputFile:	program (program)
	
	program:	statement program (statement::program)
			|	statement ([statement])
	
	statement:	formula TERM (formula)

	formula:	BOOL (if BOOL = "TRUE" then AST.BoolExp true else AST.BoolExp false)
			|	NUM (AST.IntExp NUM)
			|	ID (AST.VarExp ID)
			|	LET ID ASSIGN formula IN formula END (AST.LetExp (AST.ValDecl (ID, formula1), formula2))
			|	IF formula THEN formula ELSE formula FI (AST.ITExp (formula1, formula2, formula3))
			|	formula IMPLIES formula (AST.BinExp (AST.IMPLIES, formula1, formula2))
			|	formula AND formula (AST.BinExp (AST.AND, formula1, formula2))
			|	formula OR formula (AST.BinExp (AST.OR, formula1, formula2))
			|	formula XOR formula (AST.BinExp (AST.XOR, formula1, formula2))
			|	formula EQUALS formula (AST.BinExp (AST.EQUALS, formula1, formula2))
			|	formula LESSTHAN formula (AST.BinExp (AST.LESSTHAN, formula1, formula2))
			|	formula GREATERTHAN formula (AST.BinExp (AST.GREATERTHAN, formula1, formula2))
			|	formula PLUS formula (AST.BinExp (AST.PLUS, formula1, formula2))
			|	formula MINUS formula (AST.BinExp (AST.MINUS, formula1, formula2))
			|	formula TIMES formula (AST.BinExp (AST.TIMES, formula1, formula2))
			|	NOT formula (AST.UnExp (AST.NOT, formula))
			|	NEGATE formula (AST.UnExp (AST.NEGATE, formula))
			|	LPAREN formula RPAREN (formula)
