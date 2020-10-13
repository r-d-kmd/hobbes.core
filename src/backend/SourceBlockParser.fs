namespace Hobbes.Parsing

module SourceBlockParser = 
    open FParsec.Primitives
    open Hobbes.Parsing.Primitives
    open YamlParser
    open YamlParser.Types

    open FParsec

    let bareDocument = BlockStyle.parser
    let parse : Parser<_> = 
        fun (stream : CharStream<State>) ->
            let result = bareDocument stream
                                        
            if result.Status = Ok then
                let value = result.Result
                let sourceConfig,error = 
                    match value with
                    | Mapping(values,_) ->
                        values
                        |> Map.toSeq
                        |> Seq.map(fun (k, v) ->
                            let key = 
                                match k with
                                Value.String key -> key
                                | _ -> failwith "The key must be a string"
                            let rec convertValue = 
                                function
                                    | Mapping(values,_) -> 
                                        values
                                        |> Map.toSeq
                                        |> Seq.map(fun (k, v) ->
                                            convertValue k, convertValue v
                                        ) |> Map.ofSeq
                                        |> AST.Value.Mapping
                                    | Sequence(values,_) -> 
                                        values
                                        |> List.map convertValue
                                        |> AST.Value.Sequence
                                    | String str -> AST.Value.String str
                                    | Boolean b -> AST.Value.Boolean b
                                    | Decimal d -> AST.Value.Decimal d
                                    | Null -> AST.Value.Null
                                    | Empty -> AST.Value.Null
                            key,convertValue v
                        ) |> Map.ofSeq, None
                    | _ -> Map.empty,(ErrorMessage.Expected "mapping") |> Some
                match error with
                None ->
                        match sourceConfig.["provider"] with
                        AST.Value.String sourceName -> 
                            Reply(AST.Source(sourceName,sourceConfig.Remove "provider"))
                        | _ -> Reply(Error,ErrorMessageList(ErrorMessage.Expected "Must include a filed called 'source'",result.Error))
                | Some e -> Reply(Error,ErrorMessageList(e,result.Error))
            else
                Reply(Error,ErrorMessageList(ErrorMessage.Other "not a well-formed block",result.Error))
        