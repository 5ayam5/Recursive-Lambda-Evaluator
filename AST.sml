structure AST =
struct
	type id = string

	datatype value = IntVal of int | BoolVal of bool
	type env = (id * value) list

	datatype binop = IMPLIES | AND | OR | XOR | EQUALS | LESSTHAN | GREATERTHAN | PLUS | MINUS | TIMES
	datatype unop = NOT | NEGATE

	datatype typ = Int | Bool | Arrow of typ * typ

	datatype 	decl	= ValDecl of id * exp
	and			exp		= BoolExp of bool
						| IntExp of int
						| VarExp of id
						| LetExp of decl * exp
						| ITExp of exp * exp * exp
						| BinExp of binop * exp * exp
						| UnExp of unop * exp
						| AppExp of id * exp
						| LambdaExp of id * typ * typ * exp
						| FuncExp of id * id * typ * typ * exp

	fun envLookup (var: id, e: env): value =
		case List.find(fn (x, _) => x = var) e of
			SOME (x, v) => v
		|	NONE		=> raise Fail "Use of undeclared variable"
	
	fun envAdd (var: id, v: value, e: env): env = (var, v)::e
end
