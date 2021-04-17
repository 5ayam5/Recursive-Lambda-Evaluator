structure AST =
struct
	type id = string

	datatype boolbool = IMPLIES | AND | OR | XOR
	datatype intint = PLUS | MINUS | TIMES
	datatype intbool = LESSTHAN | GREATERTHAN
	datatype binop = EQUALS | BB of boolbool | II of intint | IB of intbool
	datatype unop = NOT | NEGATE

	datatype typ = Int | Bool | Arrow of typ * typ

	datatype 	value = IntVal of int | BoolVal of bool | Lambda of id * typ * typ * exp
	and			decl	= ValDecl of id * exp
	and			exp		= BoolExp of bool
						| IntExp of int
						| VarExp of id
						| LetExp of decl * exp
						| ITExp of exp * exp * exp
						| BinExp of binop * exp * exp
						| UnExp of unop * exp
						| AppExp of id * exp
						| LambdaExp of value
						| FuncExp of id * value

	type env = (id * value) list

	fun envLookup (var: id, e: env): value =
		case List.find(fn (x, _) => x = var) e of
			SOME (x, v) => v
		|	NONE		=> raise Fail "Use of undeclared variable"
	
	fun envAdd (var: id, v: value, e: env): env = (var, v)::e

	fun typeCheckExp (expr: exp, e: env): typ * env =
		case expr of
			BoolExp b	=> (Bool, e)
		|	IntExp i	=> (Int, e)
		|	VarExp x =>
				(case envLookup (x, e) of
					IntVal i	=> (Int, e)
				|	BoolVal b	=> (Bool, e)
				|	Lambda (y, typ1, typ2, exp1) => (Arrow (typ1, typ2), e))
		|	LetExp (ValDecl (x, v), exp1) =>
				let
					val ret =
						case typeCheckExp (v, e) of
							(Int, _)				=> IntVal 0
						|	(Bool, _)				=> BoolVal false
						|	(Arrow (typ1, typ2), _)	=> Lambda ("_", typ1, typ2, v)
					val (typ1, _) = typeCheckExp (exp1, envAdd (x, ret, e))
				in
					(typ1, e)
				end
		|	ITExp (exp1, exp2, exp3) =>
				(case typeCheckExp (exp1, e) of
					(Int, _)			=> raise Fail "Type check failed: predicate not of type boolean"
				|	(Arrow (_, _), _)	=> raise Fail "Type check failed: predicate not of type boolean"
				|	(Bool, _) =>
						let
							val (ret2, _) = typeCheckExp (exp2, e)
							val (ret3, _) = typeCheckExp (exp3, e)
						in
							case (ret2, ret3) of
								(Int, Int)				=> (Int, e)
							|	(Bool, Bool)			=> (Bool, e)
							|	(Arrow a1, Arrow a2)	=> if a1 = a2 then (Arrow a1, e) else raise Fail "Type check failed: type mismatch in if branches"
							|	(_, _)					=> raise Fail "Type check failed: type mismatch in if branches"
						end)
		|	BinExp (biop, exp1, exp2) =>
				let
					val (ret1, _) = typeCheckExp (exp1, e)
					val (ret2, _) = typeCheckExp (exp2, e)
				in
					case (ret1, ret2) of
						(Int, Int)		=> (case biop of
												II _	=> (Int, e)
											|	IB _	=> (Bool, e)
											|	EQUALS	=> (Bool, e)
											|	BB _	=> raise Fail "Type check failed: type mismatch for binop, int operands for bool operator")
					|	(Bool, Bool)	=> (case biop of
												BB _	=> (Bool, e)
											|	EQUALS	=> (Bool, e)
											|	_		=> raise Fail "Type check failed: type mismatch for binop, bool operands for int operator")
					|	(_, _)			=> raise Fail "Type check failed: type mismatch for binop, invalid left and right operands"
				end
		|	UnExp (uop, exp1) =>
				(case uop of
					NOT		=> (case typeCheckExp (exp1, e) of
									(Bool, _)	=> (Bool, e)
								|	_			=> raise Fail "Type check failed: type mismatch for unop, incorrect operand for bool operator")
				|	NEGATE	=> (case typeCheckExp (exp1, e) of
									(Int, _)	=> (Int, e)
								|	_			=> raise Fail "Type check failed: type mismatch for unop, incorrect operand for int operator"))
		|	AppExp (f, exp1) =>
				let
					val (ret, _) = typeCheckExp (exp1, e)
				in
					case envLookup (f, e) of
						Lambda (_, typ1, typ2, _) => if typ1 = ret then (typ2, e) else raise Fail "Function applied to wrong argument"
					|	_ => raise Fail "ID not of type function"	
				end
		|	LambdaExp v =>
				(case v of
					Lambda (_, typ1, typ2, _) => (Arrow (typ1, typ2), e)
				|	_ => raise Fail "Not a lambda application")
		|	FuncExp (f, v) =>
				(case v of
					Lambda (y, typ1, typ2, exp1) => (Arrow (typ1, typ2), envAdd (f, Lambda (y, typ1, typ2, exp1), e))
				|	_ => raise Fail "Not a function declaration")
	
	fun	typeCheckList (nil: exp list, e: env): bool = true
	|	typeCheckList (h::t: exp list, e: env): bool =
			let
				val (_, eNew) = typeCheckExp (h, e)
			in
				typeCheckList (t, eNew)
			end

	fun evalExp (expr: exp, e: env): value * env =
		case expr of
			BoolExp b	=> (BoolVal b, e)
		|	IntExp i	=> (IntVal i, e)
		|	VarExp x	=> (envLookup (x, e), e)
		|	LetExp (ValDecl (x, v), exp1) =>
				let
					val (ret1, _) = evalExp (v, e)
					val (ret2, _) = evalExp (exp1, envAdd (x, ret1, e))
				in
					(ret2, e)
				end
		|	ITExp (exp1, exp2, exp3) =>
				(case evalExp (exp1, e) of
					(BoolVal true, _)	=> evalExp (exp2, e)
				|	(BoolVal false, _)	=> evalExp (exp3, e)
				|	_ => raise Fail "Type fail, uncaught during type checking")
		|	BinExp (biop, exp1, exp2) =>
				(case (evalExp (exp1, e), evalExp (exp2, e)) of
					((BoolVal b1, _), (BoolVal b2, _))	=>
						(case biop of
							EQUALS	=> (BoolVal (b1 = b2), e)
						|	BB bop	=>
								(case bop of
									IMPLIES	=> (BoolVal (not b1 orelse b2), e)
								|	AND		=> (BoolVal (b1 andalso b2), e)
								|	OR		=> (BoolVal (b1 orelse b2), e)
								|	XOR		=> (BoolVal (b1 <> b2), e))
						|	_ => raise Fail "Type fail, uncaught during type checking")
				|	((IntVal i, _), (IntVal j, _))		=>
						(case biop of
							EQUALS	=> (BoolVal (i = j), e)
						|	IB bop	=>
								(case bop of
									LESSTHAN	=> (BoolVal (i < j), e)
								|	GREATERTHAN	=> (BoolVal (i > j), e))
						|	II bop	=>
								(case bop of
									PLUS	=> (IntVal (i + j), e)
								|	MINUS	=> (IntVal (i - j), e)
								|	TIMES	=> (IntVal (i * j), e))
						|	_ => raise Fail "Type fail, uncaught during type checking")
				| _ => raise Fail "Type fail, uncaught during type checking")
		|	UnExp (uop, exp1) =>
				(case uop of
					NOT		=>
						(case evalExp (exp1, e) of
							(BoolVal b, _) => (BoolVal (not b), e)
						|	_ => raise Fail "Type fail, uncaught during type checking")
				|	NEGATE	=>
						(case evalExp (exp1, e) of
							(IntVal i, _) => (IntVal (~i), e)
						|	_ => raise Fail "Type fail, uncaught during type checking"))
		|	AppExp (f, exp1) =>
				(case envLookup (f, e) of
					Lambda (x, _, _, exp2) =>
						let
							val (ret1, _) = evalExp (exp1, e)
							val (ret2, _) = evalExp (exp2, envAdd (x, ret1, e))
						in
							(ret2, e)
						end
				|	_ => raise Fail "Type fail, uncaught during type checking")
		|	LambdaExp v => (v, e)
		|	FuncExp (f, v) => (v, envAdd (f, v, e))
	
	fun	evalList (nil: exp list, e: env): value list = []
	|	evalList (h::t: exp list, e: env): value list =
			let
				val (ret, eNew) = evalExp (h, e)
			in
				ret::evalList (t, eNew)
			end
end
