namespace Hobbes.Parsing

module BlockParser = 
    open FParsec.Primitives
    open FParsec.CharParsers
    open Hobbes.Parsing.StatementParser
    open Hobbes.Parsing.Primitives

    let private blockEnd =
        attempt ((newline .>> newline) >>. preturn () <|> eof)
    let statementBlock = 
        statements .>> blockEnd >>= (AST.Statements >> preturn)
    let commentBlockEnd = skipString "!#" >>. blockEnd
    let commentBlock =
        skipString "!#" >>. 
            (manyCharsTill anyChar commentBlockEnd) >>= (AST.Comment >> preturn)

    let private pblock : Parser<_> = 
       (attempt SourceBlockParser.parse)
       <|> (attempt commentBlock)
       <|> statementBlock

    let parse (input : string) = 
        let delim = System.Environment.NewLine + System.Environment.NewLine + System.Environment.NewLine
        
        let errors, blocks = 
           input.Split delim
           |> Array.map(fun block ->
               block,block.TrimStart()
                     |> run pblock
           ) |> List.ofArray
           |> List.partition(fun (block,res) ->
                match res with
                Failure _ ->
                    printfn "Failing block %s" block
                    true 
                | Success _ ->
                    false
           )

        let errors = 
            errors
            |> Seq.map(fun (_,res) ->
                match res with
                Failure(msg,e,_) ->
                    sprintf "Line: %d, Col: %d\t %s" e.Position.Line e.Position.Column msg
                | _ ->
                    failwith "Not an parse failure"
            ) |> List.ofSeq

        let blocks = 
            blocks
            |> Seq.map(fun (_,res) ->
                match res with
                Failure (msg,e,_) ->
                    failwithf "Not a successful parsed block. Line: %d, Col: %d\t %s" e.Position.Line e.Position.Column msg
                | Success(block,_, _ ) ->
                    block
            ) |> List.ofSeq

        errors,blocks