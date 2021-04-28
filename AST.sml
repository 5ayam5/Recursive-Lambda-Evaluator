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
						| AppExp of exp * exp
						| LambdaExp of value
						| FuncExp of id * value

	type env = (id * value) list
	type typEnv = (id * typ) list

	val formulaNumber = ref(0)

	fun envLookup (var: id, e: env): value =
		case List.find(fn (x, _) => x = var) e of
			SOME (x, v) => v
		|	NONE		=> raise Fail ("Use of undeclared variable: " ^ var ^ "\n")

	fun typEnvLookup (var: id, e: typEnv): typ =
		case List.find(fn (x, _) => x = var) e of
			SOME (x, v) => v
		|	NONE		=> raise Fail ("Use of undeclared variable: " ^ var ^ "\n")

	fun envAdd (var: id, v: value, e: env): env = (var, v)::e

	fun typEnvAdd (var: id, v: typ, e: typEnv): typEnv = (var, v)::e

	fun typToString (ty: typ): string =
		case ty of
			Int		=> "int"
		|	Bool	=> "bool"
		|	Arrow (typ1, typ2) => "(" ^ typToString typ1 ^ " -> " ^ typToString typ2 ^ ")"

	fun typeCheckExp (expr: exp, e: typEnv): typ * typEnv =
		case expr of
			BoolExp b	=> (Bool, e)
		|	IntExp i	=> (Int, e)
		|	VarExp x => (typEnvLookup (x, e), e)
		|	LetExp (ValDecl (x, v), exp1) =>
				let
					val (ret, _) = typeCheckExp (v, e)
					val (typ1, _) = typeCheckExp (exp1, typEnvAdd (x, ret, e))
				in
					(typ1, e)
				end
		|	ITExp (exp1, exp2, exp3) =>
				(case typeCheckExp (exp1, e) of
					(Bool, _) =>
						let
							val (ret2, _) = typeCheckExp (exp2, e)
							val (ret3, _) = typeCheckExp (exp3, e)
						in
							case (ret2, ret3) of
								(Int, Int)				=> (Int, e)
							|	(Bool, Bool)			=> (Bool, e)
							|	(Arrow a1, Arrow a2)	=> if a1 = a2 then (Arrow a1, e) else raise Fail "Type check failed: type mismatch in if branches\n"
							|	_ => raise Fail "Type check failed: type mismatch in if branches\n"
						end
				|	_ => raise Fail "Type check failed: predicate not of type boolean\n")
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
											|	BB _	=> raise Fail "Type check failed: type mismatch for binop, int operands for bool operator\n")
					|	(Bool, Bool)	=> (case biop of
												BB _	=> (Bool, e)
											|	EQUALS	=> (Bool, e)
											|	_		=> raise Fail "Type check failed: type mismatch for binop, bool operands for int operator\n")
					|	_ => raise Fail "Type check failed: type mismatch for binop, invalid left and right operands\n"
				end
		|	UnExp (uop, exp1) =>
				(case uop of
					NOT		=> (case typeCheckExp (exp1, e) of
									(Bool, _)	=> (Bool, e)
								|	_			=> raise Fail "Type check failed: type mismatch for unop, incorrect operand for bool operator\n")
				|	NEGATE	=> (case typeCheckExp (exp1, e) of
									(Int, _)	=> (Int, e)
								|	_			=> raise Fail "Type check failed: type mismatch for unop, incorrect operand for int operator\n"))
		|	AppExp (f, exp1) =>
				(case typeCheckExp (f, e) of
					(Arrow (typ1, typ2), _) =>
						let
							val (ret, _) = typeCheckExp (exp1, e)
						in
							if typ1 = ret then (typ2, e) else raise Fail "Type check failed: function applied to wrong argument\n"
						end
				|	_ => raise Fail "Type check failed: not a function application\n")
		|	LambdaExp v =>
				(case v of
					Lambda (x, typ1, typ2, exp1) =>
						let
							val (ret, _) = typeCheckExp (exp1, typEnvAdd (x, typ1, e))
						in
							if ret = typ2 then (Arrow (typ1, typ2), e) else raise Fail "Type check failed: expected and evaluated return type mismatch\n"
						end
				|	_ => raise Fail "Type check failed: not a lambda application\n")
		|	FuncExp (f, v) =>
				(case v of
					Lambda (x, typ1, typ2, exp1) => typeCheckExp (LambdaExp (Lambda (x, typ1, typ2, exp1)), typEnvAdd (f, Arrow (typ1, typ2), e))
				|	_ => raise Fail "Type check failed: not a function declaration\n")

	fun	typeCheckList (nil: exp list, e: typEnv): bool = true
	|	typeCheckList (h::t: exp list, e: typEnv): bool =
			let
				val (_, eNew) = typeCheckExp (h, e)
			in
				(formulaNumber := !formulaNumber + 1; typeCheckList (t, eNew))
			end

	fun evaluateLambda (x: id, v: value, expr: exp): exp =
		case expr of
			VarExp y => if x = y then
				(case v of
					IntVal i	=> IntExp i
				|	BoolVal b	=> BoolExp b
				|	l			=> LambdaExp l)
				else VarExp y
		|	LetExp (ValDecl (y, exp1), exp2) => LetExp (ValDecl (y, exp1), if x = y then exp2 else evaluateLambda (x, v, exp2))
		|	ITExp (exp1, exp2, exp3) => ITExp (evaluateLambda (x, v, exp1), evaluateLambda (x, v, exp2), evaluateLambda (x, v, exp3))
		|	BinExp (biop, exp1, exp2) => BinExp (biop, evaluateLambda (x, v, exp1), evaluateLambda (x, v, exp2))
		|	UnExp (uop, exp1) => UnExp (uop, evaluateLambda (x, v, exp1))
		|	AppExp (f, exp1) => AppExp (evaluateLambda (x, v, f), evaluateLambda (x, v, exp1))
		|	LambdaExp (Lambda (y, typ1, typ2, exp1)) => LambdaExp (Lambda (y, typ1, typ2, if x = y then exp1 else evaluateLambda (x, v, exp1)))
		|	FuncExp (f, v2) => if x = f then FuncExp (f, v2) else
				(case v2 of
					Lambda (y, typ1, typ2, exp2) => FuncExp (f, Lambda (y, typ1, typ2, if x = y then exp2 else evaluateLambda (x, v, exp2)))
				|	_ => raise Fail "Type fail, uncaught during type checking\n")
		|	some => some

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
				|	_ => raise Fail "Type fail, uncaught during type checking\n")
		|	BinExp (biop, exp1, exp2) =>
				(case (evalExp (exp1, e), evalExp (exp2, e)) of
					((BoolVal b1, _), (BoolVal b2, _))	=>
						(case biop of
							EQUALS	=> BoolVal (b1 = b2)
						|	BB bop	=>
								(case bop of
									IMPLIES	=> BoolVal (not b1 orelse b2)
								|	AND		=> BoolVal (b1 andalso b2)
								|	OR		=> BoolVal (b1 orelse b2)
								|	XOR		=> BoolVal (b1 <> b2))
						|	_ => raise Fail "Type fail, uncaught during type checking\n")
				|	((IntVal i, _), (IntVal j, _))		=>
						(case biop of
							EQUALS	=> BoolVal (i = j)
						|	IB bop	=>
								(case bop of
									LESSTHAN	=> BoolVal (i < j)
								|	GREATERTHAN	=> BoolVal (i > j))
						|	II bop	=>
								(case bop of
									PLUS	=> IntVal (i + j)
								|	MINUS	=> IntVal (i - j)
								|	TIMES	=> IntVal (i * j))
						|	_ => raise Fail "Type fail, uncaught during type checking\n")
				| _ => raise Fail "Type fail, uncaught during type checking\n", e)
		|	UnExp (uop, exp1) =>
				(case uop of
					NOT		=>
						(case evalExp (exp1, e) of
							(BoolVal b, _) => (BoolVal (not b), e)
						|	_ => raise Fail "Type fail, uncaught during type checking\n")
				|	NEGATE	=>
						(case evalExp (exp1, e) of
							(IntVal i, _) => (IntVal (~i), e)
						|	_ => raise Fail "Type fail, uncaught during type checking\n"))
		|	AppExp (f, exp1) =>
				(case evalExp (f, e) of
					(Lambda (x, _, _, exp2), _) =>
						let
							val (ret1, _) = evalExp (exp1, e)
							val (ret2, _) = evalExp (evaluateLambda (x, ret1, exp2), e)
						in
							(ret2, e)
						end
				|	_ => raise Fail "Type fail, uncaught during type checking\n")
					
		|	LambdaExp v => (v, e)
		|	FuncExp (f, v) => (v, envAdd (f, v, e))

	fun	evalList (nil: exp list, e: env): value list = []
	|	evalList (h::t: exp list, e: env): value list =
			let
				val (ret, eNew) = evalExp (h, e)
			in
				ret::evalList (t, eNew)
			end

	fun	printList (nil: value list) = print ""
	|	printList (h::t: value list) =
			(case h of
				IntVal i	=> print ("IntVal " ^ Int.toString i ^ "\n")
			|	BoolVal b	=> print ("BoolVal " ^ Bool.toString b ^ "\n")
			|	Lambda (x, typ1, typ2, exp) => print ("Lambda of " ^ x ^ ": " ^ typToString typ1 ^ ", return type: " ^ typToString typ2 ^ "\n");
			printList t)
end
