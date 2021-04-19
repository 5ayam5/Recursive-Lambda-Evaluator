CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "AST.sml";
use "parser.yacc.sig";
use "parser.yacc.sml";
use "lexer.lex.sml";
use "main.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
