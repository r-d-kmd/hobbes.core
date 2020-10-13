namespace Hobbes.Parsing

module StatementParser = 
    open FParsec.Primitives
    open FParsec.CharParsers
    open Hobbes.Parsing.Expressions
    open Hobbes.Parsing.Primitives

    let private indexByColumn = kwIndex >>. kwRows >>. kwBy >>. expression >>= (AST.IndexBy >> preturn)
    let private sortByColumns = kwSort  >>. kwBy  >>. kwColumn >>. columnName >>= (AST.SortBy >> preturn)
    let private sliceColumns = kwSlice  >>. kwColumns  >>. columnNameList >>= (AST.SliceColumns >> preturn)
    let private dense = kwDense >>?  (kwColumns  >>= (fun _ -> AST.DenseColumns |> preturn) <|> (kwRows >>= (fun _ -> AST.DenseRows |> preturn)))
    let private numericColumns = kwNumeric >>? kwColumns >>= (fun _ -> AST.NumericColumns |> preturn)
    let private only = 
        kwOnly >>. expression >>= (
            checkBooleanExp
            >> AST.Only 
            >> preturn)
    
    let filtering = 
        indexByColumn <|>
        sortByColumns <|>
        sliceColumns <|>
        dense <|> 
        numericColumns <|>
        only
        >>= (AST.FilterAndSorting >> preturn)

    let pivot = 
        pipe4 (kwPivot >>. expressionInBrackets  .>> spaces) 
             (expressionInBrackets  .>> spaces)
             (opArrow >>. reduction) 
             (expressionInBrackets) 
             (fun rowKeyExpression columnKeyExpression reduction valueExpression ->
            AST.Pivot(rowKeyExpression,columnKeyExpression,valueExpression, reduction)
        )
   
    let createColumn = 
        pipe2 (kwCreate .>> kwColumn >>. columnName) expression (fun columnName exp  -> AST.CreateColumn(exp,columnName))

    let renameColumn = 
        pipe2 (kwRename .>> kwColumn >>. columnName) (spaces >>. columnName)  (fun orgColumnName newColumnName  -> AST.RenameColumn(orgColumnName,newColumnName))
    
    let column = 
       pivot <|>
       renameColumn <|> 
       createColumn
       >>= (AST.Column >> preturn)

    
    let private selector = 
        (kwMaxBy >>. expression >>= (AST.MaxBy >> preturn))
        <|> (kwMinBy >>. expression >>= (AST.MinBy >> preturn))
        
    let private groupExpression = 
        kwGroup >>. kwBy >>. 
            pipe2 (columnNameList .>> opArrow)
                  ((selector >>= (AST.Select >> preturn)) <|> (reduction >>= (AST.Reduce >> preturn)))
                  (fun columnNames grpReduction ->
                     AST.GroupBy(columnNames, grpReduction)
                  )
   
    let private distinctExpression = 
        kwEach >>. reduction >>= (AST.Each >> preturn)

    let private clusteringExpression kw ast = 
        kw >>. pipe2 (pint32 .>> (spaces1 <|> opArrow)) reduction  (fun buckets reduction -> ast(buckets, reduction))
        
    let private bucketsExpression = 
        clusteringExpression kwBuckets AST.Buckets

    let private kmeansExpression = 
        clusteringExpression kwKMeans AST.KMeansClustering

    let clustering = 
        groupExpression <|>
        distinctExpression <|>
        bucketsExpression <|> 
        kmeansExpression
        >>= (AST.Cluster >> preturn)
    
    let statement = 
        column <|>
        filtering <|>
        clustering 
    
    let statements : Parser<_> = 
        many (statement .>> eol )

    let parse (input : string) = 
        match run statements input with
        Failure(msg,e,_) ->
            failwithf "Line: %d, Col: %d\t %s" e.Position.Line e.Position.Column msg
        | Success(res,_,_)->
            res