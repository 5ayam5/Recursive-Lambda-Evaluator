structure A3LrVals = A3LrValsFun(structure Token = LrParser.Token);
structure A3Lex = A3LexFun(structure Tokens = A3LrVals.Tokens);
structure A3Parser =
	Join(structure LrParser = LrParser
		structure ParserData = A3LrVals.ParserData
		structure Lex = A3Lex);

fun main () =
		let
			fun invoke lexstream =
					let
						fun print_error (s, line, col) =
								TextIO.output(TextIO.stdErr, "Syntax Error:" ^ (Int.toString line) ^ ":" ^ (Int.toString col) ^ ":" ^ s ^ "\n");
					in
						A3Parser.parse(0,lexstream,print_error,())
					end

			fun stringToLexer str =
					let
						val done = ref false
						val lexer =  A3Parser.makeLexer (fn _ => if (!done) then "" else (done := true; str))
					in
						lexer
					end	
		
			fun parse lexer =
					let
						val dummyEOF = A3LrVals.Tokens.EOF(0,0)
						val (result, lexer) = invoke lexer
						val (nextToken, lexer) = A3Parser.Stream.get lexer
					in
						if A3Parser.sameToken(nextToken, dummyEOF) then result
						else (print("Warning: Unconsumed input \n"); result)
					end

			val fileName = case CommandLine.arguments() of h::t => h | nil => "in"
			val inputStream = TextIO.openIn fileName
			val str = TextIO.input inputStream
			val _ = TextIO.closeIn inputStream

			val programList = parse (stringToLexer str)
			val _ = (AST.formulaNumber := 0; AST.typeCheckList (programList, []))
			val eval = AST.evalList (programList, [])
		in
			AST.printList eval
		end
		handle Fail s => print ("Formula number: " ^ Int.toString (!AST.formulaNumber) ^ "\n" ^ s)

val _ = main ();
