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
	val eof = fn _ => Tokens.EOF(!line, !col)
	val error = fn (e, line, col) => TextIO.output(TextIO.stdErr, "Unknown token:" ^ (Int.toString line) ^ ":" ^ (Int.toString col) ^ ":" ^ e ^ "\n")


%%
%header (functor A3LexFun(structure Tokens:A3_TOKENS));
alpha=[A-Za-z];
num=[0-9];
ws = [\ \t];
%%
\n				=> (line := (!line) + 1; eolpos := yypos + size yytext; lex());
{ws}+			=> (lex());
";"				=> (col := yypos - (!eolpos); Tokens.TERM(!line, !col));
"if"			=> (col := yypos - (!eolpos); Tokens.IF(!line, !col));
"then"			=> (col := yypos - (!eolpos); Tokens.THEN(!line, !col));
"else"			=> (col := yypos - (!eolpos); Tokens.ELSE(!line, !col));
"fi"			=> (col := yypos - (!eolpos); Tokens.FI(!line, !col));
"IMPLIES"		=> (col := yypos - (!eolpos); Tokens.IMPLIES(!line, !col));
"NOT"			=> (col := yypos - (!eolpos); Tokens.NOT(!line, !col));
"("				=> (col := yypos - (!eolpos); Tokens.LPAREN(!line, !col));
")"				=> (col := yypos - (!eolpos); Tokens.RPAREN(!line, !col));
"AND"			=> (col := yypos - (!eolpos); Tokens.AND(!line, !col));
"OR"			=> (col := yypos - (!eolpos); Tokens.OR(!line, !col));
"XOR"			=> (col := yypos - (!eolpos); Tokens.XOR(!line, !col));
"EQUALS"		=> (col := yypos - (!eolpos); Tokens.EQUALS(!line, !col));
"TRUE"			=> (col := yypos - (!eolpos); Tokens.BOOL(yytext, !line, !col));
"FALSE"			=> (col := yypos - (!eolpos); Tokens.BOOL(yytext, !line, !col));
"PLUS"			=> (col := yypos - (!eolpos); Tokens.PLUS(!line, !col));
"MINUS"			=> (col := yypos - (!eolpos); Tokens.MINUS(!line, !col));
"TIMES"			=> (col := yypos - (!eolpos); Tokens.TIMES(!line, !col));
"NEGATE"		=> (col := yypos - (!eolpos); Tokens.NEGATE(!line, !col));
"LESSTHAN"		=> (col := yypos - (!eolpos); Tokens.LESSTHAN(!line, !col));
"GREATERTHAN"	=> (col := yypos - (!eolpos); Tokens.GREATERTHAN(!line, !col));
"let"			=> (col := yypos - (!eolpos); Tokens.LET(!line, !col));
"="				=> (col := yypos - (!eolpos); Tokens.ASSIGN(!line, !col));
"in"			=> (col := yypos - (!eolpos); Tokens.IN(!line, !col));
"end"			=> (col := yypos - (!eolpos); Tokens.END(!line, !col));
{alpha}+		=> (col := yypos - (!eolpos); Tokens.ID(yytext, !line, !col));
{num}+			=> (col := yypos - (!eolpos); Tokens.NUM(valOf (Int.fromString yytext), !line, !col));
.				=> (col := yypos - (!eolpos); error(yytext, !line, !col); raise LexError);
