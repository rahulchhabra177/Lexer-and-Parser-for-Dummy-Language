structure parser_values = booleanLrValsFun(structure Token = LrParser.Token)
structure lexer_c = CalculateLexFun(structure Tokens = parser_values.Tokens);
structure Parser =
      Join(structure LrParser = LrParser
               structure ParserData = parser_values.ParserData
               structure Lex = lexer_c)
(*Standard declarations which are required in a usual load file*)
   
fun wake l_stream =
                let   
                 fun print_error (s,pos,col) =
                TextIO.output(TextIO.stdOut, "]\nSyntax Error:" ^ (Int.toString pos) ^ ":" ^(Int.toString col) ^":"^ s ^ "\n")
        in
            (print"[";Parser.parse(0,l_stream,print_error,()))


        end

fun stringToLexer infile =

    let
        val instream= TextIO.openIn infile (*making instream from the inputfile*)
        val str=TextIO.inputAll instream(*Storing the input in a string*)
    in  let
        val done = ref false
        val lexer=  Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
    lexer
    end 
end
        
fun parse (lexer) =
    let val dummyEOF = parser_values.Tokens.EOF(0,0)
        val (result, lexer) = (wake lexer)
    val (nextToken, lexer) = (Parser.Stream.get lexer)
    in
        if (Parser.sameToken(nextToken, dummyEOF)) then (result)
    else (TextIO.output(TextIO.stdOut, "Input not Fully Consumed "); result)
    end

val parseString = parse o stringToLexer



