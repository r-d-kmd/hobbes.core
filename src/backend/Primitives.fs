namespace Hobbes.Parsing

[<AutoOpen>]
module Primitives = 
    open FParsec
    open YamlParser.Types

    type Parser<'a> = Parser<'a,YamlParser.Types.State> 
    let spaces : Parser<_> = skipMany (skipAnyOf " \t\r")
    let spaces1 : Parser<_> = skipMany1 (skipAnyOf " \t\r")
    let private identifier : Parser<_> = identifier (IdentifierOptions())
    let eof : Parser<_>=
        fun stream ->
            if stream.IsEndOfStream then Reply(())
            else Reply(Error, ErrorMessageList(ErrorMessage.Expected "eof"))

    let rec eol : Parser<_> = 
        fun stream ->
            if stream.IsEndOfStream then Reply(())
            elif stream.SkipNewline() then Reply(())
            else 
                (skipAnyOf " \t\r" .>> eol) stream

    let stringThenWhiteSpace s = pstring s >>. spaces
    let stringLiteral<'u> : Parser<string, 'u> =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        between (pstring "\"") (pstring "\"")
                (manyChars (normalChar <|> escapedChar)) 

    let delimitedString<'u> (delim : string) escaped : Parser<string, 'u> =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> delim.[0])
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf (delim + escaped) |>> unescape)
        between (pstring delim) (pstring delim)
                (manyChars (normalChar <|> escapedChar))
    
    let pquotedStringLiteral : Parser<_> =
        delimitedString "\'" "\\nrt"

    let quotedStringLiteral : Parser<AST.ComputationExpression, YamlParser.Types.State> = 
        pquotedStringLiteral >>= ((fun (s : string) -> 
             match System.DateTime.TryParse(s,System.Globalization.CultureInfo.CurrentCulture,System.Globalization.DateTimeStyles.None) with
             true, v  -> AST.DateTime v
             | false, _ -> AST.String s
        ) >> preturn)
        
    let regexLiteral : Parser<_> = 
        delimitedString "/" "\\abtrvfnexcudw.$^{[(|)*+?" .>> spaces

    //someName || "some quoted string"
    let columnName = stringLiteral <|> identifier 
    //col1 "col2" "column three"
    let columnNameList = many1 (columnName .>> spaces) 
        // We want to support decimal or hexadecimal numbers with an optional minus
        // sign. Integers may have an 'L' suffix to indicate that the number should
        // be parsed as a 64-bit integer.
    let numberFormat =     
        NumberLiteralOptions.AllowMinusSign
        ||| NumberLiteralOptions.AllowFraction

    let private number : Parser<_> =
        let parser = numberLiteral numberFormat "number"
        fun stream ->
            let reply = parser stream
            if reply.Status = Ok then
                let nl = reply.Result // the parsed NumberLiteral
                if nl.SuffixLength = 0
                   || (   nl.IsInteger
                       && nl.SuffixLength = 1 && nl.SuffixChar1 = 'L')
                then
                    try
                        let result = if nl.IsInteger then
                                         if nl.SuffixLength = 0 then
                                             AST.Int32 (int32 nl.String)
                                         else
                                             AST.Int64 (int64 nl.String)
                                     else
                                         if nl.IsHexadecimal then
                                             AST.Float (floatOfHexString nl.String)
                                         else
                                             AST.Float (float nl.String)
                        Reply(result)
                    with
                    | :? System.OverflowException as e ->
                        stream.Skip(-nl.String.Length)
                        Reply(FatalError, messageError e.Message)
                else
                    stream.Skip(-nl.SuffixLength)
                    Reply(Error, messageError "invalid number suffix")
            else // reconstruct error reply
                Reply(reply.Status, reply.Error)
    let pnumber : Parser<_> = number
    let run p = 
       let state  =
           { 
              indent      = 0L
              indentType  = AutoDetect
              context     = BlockIn
              chomping    = None 
           }
       runParserOnString p state ""