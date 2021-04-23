build: lexer.lex.sml parser.yacc.* a3

a3: a3.mlb lexer.lex.sml parser.yacc.* AST.sml main.sml
	mlton a3.mlb

load: lexer.lex.sml parser.yacc.*
	rlwrap sml loader.sml

lexer.lex.sml: lexer.lex
	mllex lexer.lex

parser.yacc.*: parser.yacc
	mlyacc parser.yacc

clean:
	rm -f lexer.lex.* parser.yacc.* a3
