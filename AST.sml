structure AST =
struct
	datatype
		node =
		EOF
	|	TERM
	|	IF
	|	THEN
	|	ELSE
	|	IMPLIES
	|	NOT
	|	LPAREN
	|	RPAREN
	|	AND
	|	OR
	|	XOR
	|	EQUALS
	|	CONST of string
	|	ID of string
	|	Production of string * node list

	fun postOrder node =
		case node of
			EOF		=> print("EOF\n")
		|	TERM	=> print("TERM\n")
		|	IF		=> print("IF\n")
		|	THEN	=> print("THEN\n")
		|	ELSE	=> print("ELSE\n")
		|	IMPLIES	=> print("IMPLIES\n")
		|	NOT		=> print("NOT\n")
		|	LPAREN	=> print("LPAREN\n")
		|	RPAREN	=> print("RPAREN\n")
		|	AND		=> print("AND\n")
		|	OR		=> print("OR\n")
		|	XOR		=> print("XOR\n")
		|	EQUALS	=> print("EQUALS\n")
		|	CONST c	=> print("CONST: " ^ c ^ "\n")
		|	ID id	=> print("ID: " ^ id ^ "\n")
		|	Production (rule, lst) =>
				let
					fun iterateList lst =
						case lst of
							nil		=> ()
						|	h::t	=> (postOrder h; iterateList t)
				in
					(iterateList lst; print(rule ^ "\n"))
				end
end
