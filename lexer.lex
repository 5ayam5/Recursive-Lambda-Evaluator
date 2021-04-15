structure Tokens = Tokens
	exception LexError
	type pos = int
	(* Position in file *)
	type svalue = Tokens.svalue
	type ('a,'b) token = ('a,'b) Tokens.token
	type lexresult = (svalue,pos) token
	type lexarg = string
	type arg = lexarg

	val line = ref 1;
	val col = ref 0;
	val eolpos = ref 0;
	val TokenList = ref [];
	val eof = fn () => 
		let
			fun revAndPrint nil = print("[")
			|	revAndPrint (h::t) = (revAndPrint t; print (h ^ ", "))
			val _ = (revAndPrint (!TokenList); print("EOF]\n"))
		in
			Tokens.EOF(!line, !col)
		end
	val error = fn (e, line, col) => TextIO.output(TextIO.stdErr, "Unknown token:" ^ (Int.toString line) ^ ":" ^ (Int.toString col) ^ ":" ^ e ^ "\n")


%%
%header (functor A3LexFun(structure Tokens:A3_TOKENS));
alpha=[A-Za-z];
ws = [\ \t];
%%
\n			=> (line := (!line) + 1; eolpos := yypos + size yytext; lex());
{ws}+		=> (lex());
";"			=> (col := yypos - (!eolpos); TokenList := "TERM \";\""::(!TokenList); Tokens.TERM(!line,!col));
"IF"		=> (col := yypos - (!eolpos); TokenList := "IF \"IF\""::(!TokenList); Tokens.IF(!line, !col));
"THEN"		=> (col := yypos - (!eolpos); TokenList := "THEN \"THEN\""::(!TokenList); Tokens.THEN(!line, !col));
"ELSE"		=> (col := yypos - (!eolpos); TokenList := "ELSE \"ELSE\""::(!TokenList); Tokens.ELSE(!line, !col));
"IMPLIES"	=> (col := yypos - (!eolpos); TokenList := "IMPLIES \"IMPLIES\""::(!TokenList); Tokens.IMPLIES(!line, !col));
"NOT"		=> (col := yypos - (!eolpos); TokenList := "NOT \"NOT\""::(!TokenList); Tokens.NOT(!line, !col));
"("			=> (col := yypos - (!eolpos); TokenList := "LPAREN \"(\""::(!TokenList); Tokens.LPAREN(!line, !col));
")"			=> (col := yypos - (!eolpos); TokenList := "RPAREN \")\""::(!TokenList); Tokens.RPAREN(!line, !col));
"AND"		=> (col := yypos - (!eolpos); TokenList := "AND \"AND\""::(!TokenList); Tokens.AND(!line, !col));
"OR"		=> (col := yypos - (!eolpos); TokenList := "OR \"OR\""::(!TokenList); Tokens.OR(!line, !col));
"XOR"		=> (col := yypos - (!eolpos); TokenList := "XOR \"XOR\""::(!TokenList); Tokens.XOR(!line, !col));
"EQUALS"	=> (col := yypos - (!eolpos); TokenList := "EQUALS \"EQUALS\""::(!TokenList); Tokens.EQUALS(!line, !col));
"TRUE"		=> (col := yypos - (!eolpos); TokenList := "CONST \"TRUE\""::(!TokenList); Tokens.CONST(yytext, !line, !col));
"FALSE"		=> (col := yypos - (!eolpos); TokenList := "CONST \"FALSE\""::(!TokenList); Tokens.CONST(yytext, !line, !col));
{alpha}+	=> (col := yypos - (!eolpos); TokenList := ("ID \"" ^ yytext ^ "\"")::(!TokenList); Tokens.ID(yytext, !line, !col));
.			=> (col := yypos - (!eolpos); error(yytext, !line, !col); raise LexError);
