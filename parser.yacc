%%
%name A3
%term
	EOF | TERM | IF | THEN | ELSE | FI | IMPLIES | NOT | LPAREN | RPAREN | AND | OR | XOR | EQUALS | PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN | LET | ASSIGN | IN | END | BOOL of string | NUM of int | ID of string | LAMBDA | FUNC | MAP | DEF | TYPDEF | TYPE of string
%nonterm
	InputFile of AST.exp list | program of AST.exp list | statement of AST.exp | exp of AST.exp | typ of AST.typ
%pos int

%start InputFile

%eop EOF
%noshift EOF

%right MAP
%right DEF
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%right NEGATE
%nonassoc ID BOOL NUM LPAREN

%verbose

%%
	InputFile:	program (program)

	program:	statement program (statement::program)
			|	exp ([exp])
			|	statement ([statement])

	statement:	exp TERM (exp)

	exp:	BOOL (if BOOL = "TRUE" then AST.BoolExp true else AST.BoolExp false)
		|	NUM (AST.IntExp NUM)
		|	ID (AST.VarExp ID)
		|	LET ID ASSIGN exp IN exp END (AST.LetExp (AST.ValDecl (ID, exp1), exp2))
		|	IF exp THEN exp ELSE exp FI (AST.ITExp (exp1, exp2, exp3))
		|	exp IMPLIES exp (AST.BinExp (AST.BB AST.IMPLIES, exp1, exp2))
		|	exp AND exp (AST.BinExp (AST.BB AST.AND, exp1, exp2))
		|	exp OR exp (AST.BinExp (AST.BB AST.OR, exp1, exp2))
		|	exp XOR exp (AST.BinExp (AST.BB AST.XOR, exp1, exp2))
		|	exp EQUALS exp (AST.BinExp (AST.EQUALS, exp1, exp2))
		|	exp LESSTHAN exp (AST.BinExp (AST.IB AST.LESSTHAN, exp1, exp2))
		|	exp GREATERTHAN exp (AST.BinExp (AST.IB AST.GREATERTHAN, exp1, exp2))
		|	exp PLUS exp (AST.BinExp (AST.II AST.PLUS, exp1, exp2))
		|	exp MINUS exp (AST.BinExp (AST.II AST.MINUS, exp1, exp2))
		|	exp TIMES exp (AST.BinExp (AST.II AST.TIMES, exp1, exp2))
		|	NOT exp (AST.UnExp (AST.NOT, exp))
		|	NEGATE exp (AST.UnExp (AST.NEGATE, exp))
		|	LPAREN exp RPAREN (exp)
		|	exp ID (AST.AppExp (exp, AST.VarExp ID))
		|	exp BOOL (AST.AppExp (exp, if BOOL = "TRUE" then AST.BoolExp true else AST.BoolExp false))
		|	exp NUM (AST.AppExp (exp, AST.IntExp NUM))
		|	exp LPAREN exp RPAREN (AST.AppExp (exp1, exp2))
		|	LAMBDA LPAREN ID TYPDEF typ RPAREN TYPDEF typ DEF exp (AST.LambdaExp (AST.Lambda (ID, typ1, typ2, exp)))
		|	FUNC ID LPAREN ID TYPDEF typ RPAREN TYPDEF typ DEF exp (AST.FuncExp (ID1, AST.Lambda (ID2, typ1, typ2, exp)))

	typ:	typ MAP typ (AST.Arrow (typ1, typ2))
		|	TYPE (if TYPE = "int" then AST.Int else AST.Bool)
		|	LPAREN typ RPAREN MAP typ (AST.Arrow (typ1, typ2))
