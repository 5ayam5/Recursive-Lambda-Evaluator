all: lexer.lex.sml parser.yacc.*
	mlton a2.mlb

lexer.lex.sml: lexer.lex
	mllex lexer.lex

parser.yacc.*: parser.yacc
	mlyacc parser.yacc

clean:
	rm lexer.lex.* parser.yacc.* a2