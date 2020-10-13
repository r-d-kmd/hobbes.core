namespace Hobbes.Tests

open Xunit
open Hobbes.DSL
open Hobbes.Parsing.AST
open Hobbes.Parsing
open FParsec.CharParsers
open Hobbes.Parsing.Primitives

module Parser =

    let parse =
        string
        >> StatementParser.parse
        >> Seq.exactlyOne

    let parseBlocks (text : string) = 
        let errors,blocks = 
            text.Trim() |> BlockParser.parse
        if errors |> List.isEmpty |> not then
           printfn "Block parse error: %s" (System.String.Join(",", errors))
        Assert.Empty(errors)
        blocks


    let parseComment (text : string) = 
        match text.TrimStart()
              |> run BlockParser.commentBlock with
        Failure(msg,e,_) ->
            printf "Line: %d, Col: %d\t %s" e.Position.Line e.Position.Column msg
            Assert.True false
            ""
        | Success(Comment cmt,_,_) ->
            cmt
        | block -> 
            printf "Unexpected block %A" block
            Assert.True false
            ""

    let buildComparisonExpr expr1 expr2 oper =
        Comparison (expr1, expr2, oper)
   
    [<Fact>]
    let ``If else``() =
        let compareState s =
            buildComparisonExpr (ColumnName "State") (ComputationExpression.String s) EqualTo
        let boolOr lhs rhs =
            Or (lhs, rhs)
        let state = !> "State"

        let ast = 
            create (column "ProgressState") (If ((state == "Ready") .|| (state == "Ready for estimate") .|| (state == "New"))
               (Then "Todo")
               (Else 
                    (If ((state == "In sprint") .|| (state == "Active" ))
                        (Then "Doing")
                        (Else "Done"))
               )) |> parse

        let expected =
            let condition = 
                compareState "New"
                |> boolOr (
                   compareState "Ready for estimate"
                   |> boolOr (compareState "Ready")
                )
            let thenBody = ComputationExpression.String ("Todo")


            let elseBody =
                let condition = 
                    compareState "Active"
                    |> boolOr (compareState "In sprint")
                let thenBody = ComputationExpression.String "Doing"
                let elseBody = ComputationExpression.String "Done"
                IfThisThenElse(condition, thenBody, elseBody)
        
            CreateColumn (IfThisThenElse(condition, thenBody, elseBody), "ProgressState") 
            |> Column
            
        Assert.Equal(ast, expected)

    [<Fact>]
    let onlyParse() =
        let actual = only (!> "Sprint" == 5) |> parse
        let expected = FilterAndSorting (Only(Comparison(ColumnName "Sprint", Number (Int32 5), EqualTo)))
        Assert.True(actual.Equals(expected))

    [<Fact>]
    let parseCommentBlock() = 
        let originalComment = 
            """!# This is a markdown title
this is then the body of the paragraph !#


"""
        let cmt = 
            originalComment 
            |> parseComment
        Assert.Equal(originalComment.TrimEnd().Trim('!','#'),cmt)
    [<Fact>]
    let commentAndStatement() =
        let originalComment = 
            """!# This is a markdown title
this is then the body of the paragraph !#



"""
        let statements = (only (NumberConstant(1.) == NumberConstant(1.)))::[(slice columns ["col1"; "col2"])]
        let input = 
            originalComment + 
             (System.String.Join(System.Environment.NewLine,
                                    statements
                                    |> List.map string))
        let blocks =
            input
            |> parseBlocks
            
        let cmt = 
            match blocks with
            | (Comment cmt)::[Statements [
                        FilterAndSorting(
                            AST.Only(
                                AST.Comparison(
                                    AST.Number (AST.Int32 1),
                                    AST.Number (AST.Int32 1),
                                    AST.EqualTo
                                )
                            )
                        )
                        FilterAndSorting (SliceColumns ["col1"; "col2"])
                    ]
                  ] -> cmt
            | a -> 
                printfn "Failed to parse:%A" a
                Assert.True false
                ""

        Assert.Equal(originalComment.TrimEnd().Trim('!','#'),cmt)

    
    [<Fact>]
    let ``Source configuration block``() =
        let configuration = 
            """provider : azure devops
id : 1
account : name
project : true


"""
        let blocks =
            configuration
            |> parseBlocks
        match blocks with
        [Source(providerName,mapping)] ->
            [
                "id",Value.Decimal 1m
                "account", Value.String "name"
                "project", Value.Boolean true
            ] |> List.iter(fun (k,v) ->
                Assert.Equal(v,mapping.[k])
            )
            Assert.Equal("azure devops",providerName)
        | a ->
            printfn "Unexpected result %A" a 
            Assert.True false

    [<Fact>]
    let ``Source configuration, comments and statements``() =
        let configuration = 
            """
provider : azure devops
id : 1
account : name
project : true



"""
        let originalComment = 
            """!# This is a markdown title
this is then the body of the paragraph !#



"""
        let statements = (only (NumberConstant(1.) == NumberConstant(1.)))::[(slice columns ["col1"; "col2"])]
        let input = 
            configuration::originalComment::(statements
                                             |> List.map string)
            |> fun l -> System.String.Join(System.Environment.NewLine,l)

        let blocks =
            input
            |> parseBlocks
        match blocks with
        Source(providerName,mapping)::(Comment cmt)::[Statements [
                        FilterAndSorting(
                            AST.Only(
                                AST.Comparison(
                                    AST.Number (AST.Int32 1),
                                    AST.Number (AST.Int32 1),
                                    AST.EqualTo
                                )
                            )
                        )
                        FilterAndSorting (SliceColumns ["col1"; "col2"])
                    ]
                  ] -> 
            [
                "id",Value.Decimal 1m
                "account", Value.String "name"
                "project", Value.Boolean true
            ] |> List.iter(fun (k,v) ->
                Assert.Equal(v,mapping.[k])
            )
            Assert.Equal("azure devops",providerName)
            Assert.Equal(originalComment.TrimEnd().Trim('!','#'),cmt)
        | a ->
            printfn "Unexpected result %A" a 
            Assert.True false


    [<Fact>]
    let ``Source configuration and statements``() =
        let input = """provider: rest
method: get
url: http://vrk.dk


only 1=1
group by FTEs -> sum
index rows by "FTEs"


"""
        
        let blocks =
            input
            |> parseBlocks
        match blocks with
        Source(providerName,mapping)::[Statements [
                        FilterAndSorting(
                            AST.Only(
                                AST.Comparison(
                                    AST.Number (AST.Int32 1),
                                    AST.Number (AST.Int32 1),
                                    AST.EqualTo
                                )
                            )
                        )
                        Cluster (GroupBy(["FTEs"],AST.GroupReduction.Reduce AST.Sum) )
                        FilterAndSorting (IndexBy(ColumnName "FTEs"))
                    ]
                  ] -> 
            [
                "url",Value.String "http://vrk.dk"
                "method", Value.String "get"
            ] |> List.iter(fun (k,v) ->
                Assert.Equal(v,mapping.[k])
            )
            Assert.Equal("rest",providerName)
        | a ->
            printfn "Unexpected result %A" a 
            Assert.True false

        