namespace Hobbes.FSharp

open Hobbes.Parsing
open Hobbes.FSharp.DataStructures
open FParsec.CharParsers

module Compile = 
    
    type Table = seq<string * seq<AST.KeyType * System.IComparable>>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Table =
        let join table1 table2 = 
           table1
           |> Seq.append table2
           
    let parsedStatements (statements : #seq<AST.Statement>) = 
        statements
        |> Seq.fold(fun f' transform -> f' >> (fun (d : IDataMatrix) -> d.Transform transform)) id
        
    let statements (input : string): IDataMatrix -> IDataMatrix = 
        match input.Trim() with
        "" -> id
        | _ ->
            input
            |> StatementParser.parse
            |> parsedStatements
    type Source = {
        ProviderName : string
        Properties : Map<string,AST.Value>
    } with static member Empty 
                 with get() = 
                     {
                         ProviderName = ""
                         Properties = Map.empty 
                     }
    type CompiledBlock =
        | Comment of string   
        | Transformation of (IDataMatrix -> IDataMatrix) 

    type CompiledChunk = 
        {
            Source : Source
            Blocks : CompiledBlock list
        } with static member Empty 
                 with get() = 
                     {
                         Source = Source.Empty
                         Blocks = []
                     }
                
    let compileBlocks blocks = 
        blocks
        |> List.fold(fun chunks block ->
            let chunk = chunks |> List.head
            
            match block with
            AST.Comment s -> 
                {
                    chunk with 
                        Blocks = (Comment s)::chunk.Blocks
                }::chunks.Tail
            | AST.Source(pn,m) -> 
                let source' = 
                    {
                        ProviderName = pn
                        Properties = m
                    }
                if chunk.Source = Source.Empty then
                    ({
                        chunk with 
                            Source = source'
                    })::chunks.Tail
                else
                    {
                        Source = source'
                        Blocks = []
                    }::chunks
            | AST.Statements stmts ->
                let trans = 
                    stmts
                    |> parsedStatements
                    
                let blocks = 
                    match chunk.Blocks with
                    (Transformation f)::tail ->
                      (f >> trans |> Transformation)::tail
                    | blocks -> (Transformation trans)::blocks
                {
                  chunk with 
                      Blocks = blocks
                }::chunks.Tail
        ) [CompiledChunk.Empty]
        |> List.map(fun c ->
            { c with Blocks = c.Blocks |> List.rev}
        )

    let compile (input : string) =
        let errors, blocks = BlockParser.parse input
        if errors|> List.isEmpty |> not then
            failwith (System.String.Join(",",errors))
        else
            compileBlocks blocks