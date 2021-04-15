%%
%name A3
%term
	EOF | TERM | IF | THEN | ELSE | IMPLIES | NOT | LPAREN | RPAREN | AND | OR | XOR | EQUALS | CONST of string | ID of string
%nonterm
	InputFile of AST.node | program of AST.node | statement of AST.node | formula of AST.node
%pos int

%start InputFile

%eop EOF
%noshift EOF

%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT

%verbose

%%
	InputFile:	program (AST.Production ("InputFile -> program", [program]))
	
	program:	statement program (AST.Production ("program -> statement program", [statement, program]))
			|	(AST.Production ("program -> epsilon", []))
	
	statement:	formula TERM (AST.Production ("statement -> formula TERM", [formula, AST.TERM]))

	formula:	NOT formula (AST.Production ("formula -> NOT formula", [AST.NOT, formula]))
			|	formula AND formula (AST.Production ("formula -> formula AND formula", [formula1, AST.AND, formula2]))
			|	formula OR formula (AST.Production ("formula -> formula OR formula", [formula1, AST.OR, formula2]))
			|	formula XOR formula (AST.Production ("formula -> formula XOR formula", [formula1, AST.XOR, formula2]))
			|	formula EQUALS formula (AST.Production ("formula -> formula EQUALS formula", [formula1, AST.EQUALS, formula2]))
			|	formula IMPLIES formula (AST.Production ("formula -> formula IMPLIES formula", [formula1, AST.IMPLIES, formula2]))
			|	IF formula THEN formula ELSE formula (AST.Production ("formula -> IF formula THEN formula ELSE formula", [AST.IF, formula1, AST.THEN, formula2, AST.ELSE, formula3]))
			|	LPAREN formula RPAREN (AST.Production ("formula -> LPAREN formula RPAREN", [AST.LPAREN, formula, AST.RPAREN]))
			|	CONST (AST.CONST CONST)
			|	ID	(AST.ID ID)
