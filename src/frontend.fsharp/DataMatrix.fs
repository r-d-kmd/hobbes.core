namespace Hobbes.FSharp

open Deedle
open Accord.MachineLearning
open Hobbes.Parsing
open System
open Accord.Statistics.Models.Regression.Linear
open Thoth.Json.Net

module Clustering = 
    let withKeys colName (frame : Frame<AST.KeyType,_>) = 
            frame?(colName) <- 
                frame.RowKeys
                |> Seq.map(fun k ->
                    match AST.KeyType.UnWrap k with
                    :? (obj list) as lst ->
                        match lst with
                        [a] -> a
                        | a::b::c::_ -> (a,b,c) :> obj
                        | a::b::_ -> (a,b) :> obj
                        | _ -> lst :> obj
                    | a -> a
                )
            frame
    let private numericTypes =
        [
            typeof<Int16>
            typeof<int>
            typeof<Int64>
            typeof<Decimal>
            typeof<Single>
            typeof<Double>
        ]

    let private isNumeric v = 
        let t = v.GetType()
        numericTypes
        |> List.contains t


    let private reattachGroupedColumns columnNames (grouped : Frame<_,_>) =
        let keys = 
            grouped.RowKeys
            |> Seq.map(AST.KeyType.UnWrap >> (fun k ->
                match k with
                :? (obj list) as l -> l
                | a -> [a]
            ))
        
        columnNames
        |> List.indexed
        |> List.fold(fun frame (i,columnName) ->
            
            let groupSeries =
                keys
                |> Seq.map(fun ks ->
                       ks |> List.item i
                ) |> Seq.zip grouped.RowKeys
                |> series
            try
                frame
                |> Frame.addCol
                    columnName
                    groupSeries
            with _ ->
                eprintfn "Problems adding %s to frame [%A]. All group columns [%A]" columnName (frame.ColumnKeys) columnNames
                assert(false)
                frame
        ) grouped
        |> Frame.denseCols
        |> Frame.ofColumns

    let internal reduceGroup reduction columnNames (frame : Frame<_,_>)  = 
         //we're removing group columns
        let allOther = 
           frame 
           |> Frame.cols
           |> Series.keys
           |> Seq.filter(fun columnName ->
              columnNames
              |> List.tryFind(fun c -> c = columnName)
              |> Option.isNone
           )
        let getCols,red =
            match reduction with
            AST.Sum->
                Frame.getNumericCols,Stats.levelSum fst
            | AST.Count  -> 
                Frame.getCols,(Stats.levelCount fst)
                >> (Series.mapValues float)
            | AST.Median -> 
                Frame.getNumericCols,Stats.levelMedian fst
            | AST.Mean-> 
                Frame.getNumericCols,Stats.levelMean fst
            | AST.StdDev-> 
                Frame.getNumericCols,Stats.levelStdDev fst
            | AST.Variance-> 
                Frame.getNumericCols,Stats.levelVariance fst
            | AST.Max-> 
                Frame.getNumericCols,Series.applyLevel fst (Stats.max)
            | AST.Min-> 
                Frame.getNumericCols,Series.applyLevel fst (Stats.min)

        frame
        |> Frame.sliceCols allOther
        |> getCols
        |> Series.mapValues red
        |> Frame.ofColumns
        |> reattachGroupedColumns columnNames

    let internal group columnNames reducer (frame : Frame<AST.KeyType,string>) : Frame<AST.KeyType,string> = 
        frame 
        |> Frame.groupRowsUsing(fun _ row -> 
            columnNames   
            |> List.map(fun columnKey -> 
                match row.TryGet columnKey with
                | OptionalValue.Missing -> AST.Missing
                | OptionalValue.Present(value) -> 
                    AST.KeyType.Create value
            )|> AST.List
        ) |> reducer columnNames
       
    let internal kmeans clusters transform frame = 
        let rows = 
            frame
            |> Frame.getNumericCols
            |> Frame.ofColumns
            |> Frame.getRows
            |> Series.mapValues(fun row -> 
                row
                |> Series.mapValues(float)
                |> Series.observations
                |> Seq.map snd
                |> Array.ofSeq
            )
            |> Series.observations
            |> Array.ofSeq

        let values = 
            rows
            |> Array.map snd 

        let algorithm = KMeans(clusters)
        let clusters = 
            algorithm.Learn(values)
            
        let lookupTable = 
           clusters.Decide(values)
           |> Array.zip (rows |> Array.map fst)

        let clusterLookup key = 
           lookupTable
           |> Array.find(fun (k,_) -> k = key)
           |> fst

        frame?cluster <-
            frame
            |> Frame.mapRows(fun rowKey _ ->
                clusterLookup rowKey
            )

        frame
        |> group ["cluster"]  (reduceGroup transform) 

    let internal counting buckets transform frame = 
        let frame = 
            frame
            |> Frame.indexRowsOrdinally
        let inEach = frame.RowCount / buckets
        frame?bucket <-
            frame
            |> Frame.mapRows(fun k _ -> k / inEach)
        
        frame
        |> Frame.mapRowKeys AST.KeyType.Create 
        |> group ["bucket"] (reduceGroup transform)
        

           
    let inline internal each transformation (frame : Frame<_,_>) = 
         counting frame.RowCount transformation frame

module DataStructures =

    let inline private jsonString (s : string) = 
        "\"" +
         s.Replace("\\","\\\\")
          .Replace("\"","\\\"") 
        + "\""
    type IDataMatrix = 
        abstract Transform : AST.Statement -> IDataMatrix
        abstract Combine : IDataMatrix -> IDataMatrix
        abstract Join : string -> IDataMatrix -> IDataMatrix
        abstract ToJson : unit -> JsonValue
        abstract RowCount : int with get
    
    type private Comp = System.IComparable

    type DataMatrix (frame : Frame<AST.KeyType,string>) =
        
        let keySeries = 
            if frame.ColumnCount > 0 then
                frame
                |> Frame.getCols
                |> Series.observations
                |> (Seq.head >> snd)
                |> Series.map(fun k _ -> (k |> AST.KeyType.UnWrap) :?> Comp)
            else
                [] |> series

        let rec compileExpression frame expr : Series<AST.KeyType,Comp> -> Series<AST.KeyType,Comp> = 
            let compileExpression = compileExpression frame
            let columnComputationExpression expression (series : Series<AST.KeyType,Comp>) : Series<AST.KeyType,Comp>= 
                let aggregate columnExpression (f : Series<AST.KeyType,Comp> -> Series<AST.KeyType,float>) = 
                         series
                         |> columnExpression
                         |> f
                         |> Series.mapValues(fun v -> v :> Comp)
                     
                match expression with
                AST.Moving(reduction, windowSize, columnExpression) -> 
                    let aggregate = aggregate  (compileExpression columnExpression)
                    let f =    
                        match reduction with 
                        AST.Sum -> 
                            aggregate (Stats.movingSum windowSize)
                        | AST.Count -> 
                             aggregate (Stats.movingCount windowSize)
                        | AST.Median -> 
                             failwith "can't use median as a moving stat"
                        | AST.Mean -> 
                             aggregate (Stats.movingMean windowSize)
                        | AST.StdDev -> 
                             aggregate (Stats.movingStdDev windowSize)
                        | AST.Variance -> 
                             aggregate (Stats.movingVariance windowSize)
                        | AST.Max -> 
                             aggregate (Stats.movingMax windowSize)
                        | AST.Min -> 
                             aggregate (Stats.movingMin windowSize)
                       
                    f 
                | AST.Expanding(reduction, columnExpression) -> 
                    let aggregate = aggregate (compileExpression columnExpression)
                    match reduction with 
                    AST.Sum -> 
                        aggregate Stats.expandingSum 
                     | AST.Count -> 
                         aggregate Stats.expandingCount 
                     | AST.Median -> 
                         failwith "can't use median as an expanding stat"
                     | AST.Mean -> 
                         aggregate Stats.expandingMean 
                     | AST.StdDev -> 
                         aggregate Stats.expandingStdDev 
                     | AST.Variance -> 
                         aggregate Stats.expandingVariance 
                     | AST.Max -> 
                         aggregate Stats.expandingMax 
                     | AST.Min -> 
                         aggregate Stats.expandingMin
                    
            match expr with
            AST.Number n ->
               let n = 
                   match n with
                     AST.Int32 n -> n :> Comp
                     | AST.Int64 n -> n :> Comp
                     | AST.Float n -> n :> Comp
               Series.mapValues(fun _ -> n)
            | AST.RegularExpression(expr, pattern, result) ->
                let regex input = 
                    System.Text.RegularExpressions.Regex.Matches(input, pattern)
                let formatter (groups : string []) = 
                    result
                    |> List.fold(fun acc token ->
                        match token with
                        AST.RegExGroupIdentifier n ->
                            match groups |> Array.tryItem n with
                            None -> acc
                            | Some g -> g::acc
                        | AST.RegExResultString literal ->
                            literal::acc
                    ) []
                    |> List.rev
                    |> System.String.Concat
                    :> Comp
                (expr
                |> compileExpression)
                >> Series.mapValues (fun v ->
                    let input = string v
                    let matches =
                        input
                        |> regex
                        |> List.ofSeq
                    let groups = 
                        match matches with 
                        [] -> 
                            []
                        | [h] -> 
                            h.Groups 
                            |> List.ofSeq
                        | _ -> 
                            matches 
                            |> List.ofSeq 
                            |> List.collect(fun m -> m.Groups |> List.ofSeq)
                    let res = 
                        groups
                        |> Seq.map(fun g -> g.Value)
                        |> Array.ofSeq
                        |> formatter
                    res
                )
            | AST.DateTime d ->
               Series.mapValues(fun _ -> d :> Comp)           
            | AST.MissingValue ->
               Series.mapValues(fun _ -> null )
            | AST.String s ->
                  Series.mapValues(fun _ -> s :> Comp )          
            | AST.Keys ->
                  Series.keys
                  >> Seq.map(fun key ->
                      key,
                      match key |> AST.KeyType.UnWrap with
                       :? (obj list) as lst ->
                           match lst with
                           [a] -> a :?> Comp
                           | _ -> failwith "Can't use list"
                       | o -> o :?> Comp) 
                  >> series
            | AST.Binary(lhs,rhs, op) -> 
                let lhsExp =
                    fun series -> 
                        compileExpression lhs series
                        |> Series.mapValues(function
                            :? float as f -> f
                            | :? int as i -> float i
                            | :? decimal as i -> float i
                            | v -> failwithf "Can't convert %A into float" v)
                let rhsExp = 
                    fun series -> 
                        compileExpression rhs series
                        |> Series.mapValues(function
                            :? float as f -> f
                            | :? int as i -> float i
                            | :? decimal as i -> float i
                            | v -> failwithf "Can't convert %A into float" v)
                let f : Series<AST.KeyType,float> -> Series<AST.KeyType,float> -> Series<AST.KeyType,float> = 
                    match op with
                    AST.Addition -> (+)
                    | AST.Subtraction -> (-)
                    | AST.Multiplication -> (*)
                    | AST.Division -> (/)
                    | AST.Modulo -> 
                        fun lhs rhs ->
                            lhs
                            |> Series.filter(fun k _ ->
                                match rhs.TryGet k with
                                OptionalValue.Missing -> false 
                                | _ -> true
                            ) |> Series.map(fun k v ->
                                    (int v) % (k |> rhs.Get |> int) |> float
                            )
                fun series ->
                    f <| lhsExp series <| (rhsExp series)
                    |> Series.mapValues(fun v -> v :> Comp)
            | AST.ColumnName name ->
                 fun _ -> 
                     let series = 
                         frame
                         |> Frame.getCol name
                     series
            | AST.Int exp ->
                fun s ->
                    s |> (compileExpression exp)
                    |> Series.observations
                    |> Seq.map(fun (k,v) ->
                          k =>
                              match v with
                              :? int as i -> i :> Comp |> Some
                              | :? float as f -> int f :> Comp |> Some
                              | a -> 
                                  match a 
                                        |> string 
                                        |> System.Double.TryParse with
                                  false,_ -> None
                                  | true,v -> 
                                      v
                                      |> int 
                                      :> Comp
                                      |> Some
                              )
                    |> Series.ofOptionalObservations
            | AST.IfThisThenElse(condition,thenBody,elseBody) ->
                let conditionExp = 
                    compileBooleanExpression condition
                let thenBodyExp = compileExpression thenBody
                let elseBodyExp = compileExpression elseBody
                
                fun series ->
                    Frame(
                        ["__if__";"__then__";"__else__"], 
                        [
                         conditionExp
                         thenBodyExp 
                         elseBodyExp
                        ]|> List.map(fun f -> f series :> ISeries<_>)
                    ) |> Frame.mapRowValues(fun row -> 
                        if row.GetAs<bool> "__if__" then row.GetAs<Comp> "__then__" else row.GetAs<Comp> "__else__"
                    )
            | AST.FormatDate(columnName, format)  ->
                let column = frame.GetColumn<obj> columnName
                fun _ ->
                    column
                    |> Series.mapValues(fun v ->
                    let date = 
                        match v with
                        :? DateTimeOffset as d -> Some d.DateTime
                        | :? string as s -> 
                            (s
                             |> System.DateTimeOffset.Parse).DateTime
                            |> Some
                        | :? DateTime as d -> d |> Some
                        | _ -> None

                    let formatter (date : System.DateTime) =
                            match format with
                            AST.Year -> date.Year :> System.IComparable
                            | AST.Month -> date.Month :> System.IComparable
                            | AST.Day -> date.Day :> System.IComparable
                            | AST.Date -> date.Date :> System.IComparable
                            | AST.Weekday -> date.DayOfWeek.ToString() :> System.IComparable
                            | AST.Week -> (date.DayOfYear / 7) :> System.IComparable
                    date
                    |> Option.bind (formatter >> Some)
                    |> Option.orElse(Some null)
                    |> Option.get
                )
                    
            | AST.Boolean b ->
                compileBooleanExpression b
            | AST.ColumnExpression(expression) ->  
                columnComputationExpression expression
            | AST.Ordinals ->
                fun s -> 
                    s
                    |> Series.observationsAll
                    |> Seq.indexed
                    |> Seq.map(fun (i,(k,_)) -> k => (i :> Comp) )
                    |> series
            | AST.Extrapolate(regressionType, knownValues, count, length) -> 
                let fitSeries : Series<AST.KeyType,_> -> Series<AST.KeyType,_> =
                    match length with
                    None -> id
                    | Some length ->
                        fun s -> 
                            let tail = 
                                s
                                |> Series.skip (max 0 (s.KeyCount - length))
                            tail
                            |> Series.take (min length (tail.KeyCount))
                            

                let knownValuesExpr = 
                    (knownValues
                    |> compileExpression)
                    >> fitSeries

                fun inputSeries ->
                    let trainingData = 
                        inputSeries
                        |> fitSeries
                    
                    assert(trainingData |> Series.countKeys > 0)

                    let transformExpressionsToVariants expr = 
                        trainingData
                        |> expr
                        |> Series.mapValues(fun c -> 
                            match c :> obj with
                            :? decimal as d -> d |> float
                            | :? float as f -> f
                            | :? int as i -> i |> float
                            | s -> failwithf "Can't convert %A to float" s
                        ) |> Series.values
                        |> Array.ofSeq

                    let keys = 
                        trainingData
                        |> Series.keys
                        |> Seq.map(fun k ->
                            match k with
                            AST.KeyType.Numbers d -> d |> float
                            | AST.KeyType.DateTime d -> d.Ticks |> float
                            | AST.KeyType.Text t when System.Double.TryParse t |> fst ->
                                k 
                                |> AST.KeyType.UnWrap
                                |> string 
                                |> System.Double.TryParse
                                |> snd
                            | _ -> failwith "Can only extrapolated based on numeric values"
                        ) |> Array.ofSeq

                    let predictedXValues = 
                            let ols = OrdinaryLeastSquares()
                            let x = Array.init (keys.Length + count) float
                            
                            let regression =  ols.Learn(x |> Array.take keys.Length, keys)

                            regression.Transform(x)
                            |> Array.skip keys.Length
                            |> Array.append keys

                    assert(predictedXValues.Length = (keys.Length + count))

                    let createKey (v: float) =
                        v |> 
                        match trainingData |> Series.keys |> Seq.head with
                        AST.KeyType.Numbers _ -> AST.KeyType.Numbers
                        | AST.KeyType.DateTime _ -> (int64 >> DateTime >> AST.KeyType.DateTime)
                        | k -> failwithf "Can't convert to key. %A" k

                    let knownValues = 
                        knownValuesExpr
                        |> transformExpressionsToVariants
                        
                    match regressionType with
                    AST.Linear ->
                        let ols = OrdinaryLeastSquares()
                        let trainingKnownData = 
                            let index = knownValues.Length - keys.Length
                            knownValues |> Array.splitAt index |> snd
                        let regression = ols.Learn(keys, trainingKnownData)    
                        regression.Transform(predictedXValues)
                        |> Array.zip predictedXValues
                        |> Array.map(fun (x,y) -> createKey x => (y :> Comp))
                        |> series

            | AST.Regression(regressionType,inputTreeNodes,outputTreeNodes) ->
                let inputExpr = 
                    inputTreeNodes
                    |> compileExpression
                let outputExprs = 
                    outputTreeNodes
                    |> compileExpression
                
                fun s ->
                    let transformExpressionsToVariants expr = 
                        s
                        |> expr
                        |> Series.mapValues(fun c -> c :> obj :?> float)
                        |> Series.values
                        |> Array.ofSeq

                    let inputs = 
                        inputExpr
                        |> transformExpressionsToVariants
                    
                    let outputs = 
                        outputExprs
                        |> transformExpressionsToVariants
                        
                    match regressionType with
                    AST.Linear ->
                        let ols = OrdinaryLeastSquares()
                        //inputs will be longer than outputs when doing forecasting
                        let x = inputs |> Array.take outputs.Length
                        let regression = ols.Learn(x, outputs)    
                        regression.Transform(inputs)
                        |> Array.zip (s
                                     |> Series.keys
                                     |> Seq.toArray)
                        |> series
                        |> Series.mapValues(fun c -> c:> Comp)

        and compileTempColumn defaultName exp (cont : Comp option -> Comp) =
                defaultName, 
                   (fun frame keySeries ->
                       frame?(defaultName) <- (
                           compileExpression frame exp keySeries)
                           |> Series.mapAll (fun _ r ->  r |> cont |> Some)
                       )
                   
        
        and compileBooleanExpression exp : Series<AST.KeyType,Comp> -> Series<AST.KeyType,Comp> = 
            let binaryOp op lhs rhs =
                 fun series -> 
                     try
                         let lhsSerie = lhs series
                         let rhsSerie = rhs series
                         let frame = 
                            Frame(["lhs";"rhs"],
                                    [
                                        lhsSerie 
                                        rhsSerie
                                    ])
                         frame
                         |> Frame.mapRowValues(fun row ->
                             let lhs = row.TryGetAs<Comp> "lhs"
                             let rhs = row.TryGetAs<Comp> "rhs"
                             let res = 
                                 match lhs, rhs with
                                 OptionalValue.Missing, _ | _, OptionalValue.Missing -> false
                                 | lhs, rhs -> op lhs.Value rhs.Value
                             res :> Comp
                         )
                     with e ->
                          failwithf "Column not found. %s. \nAvailable Columns %s" e.Message (System.String.Join(",",frame.ColumnKeys |> Seq.map (sprintf "%A")))
            match exp with
            AST.Not e -> 
                let exp = compileBooleanExpression e
                fun series -> 
                    exp series
                    |> Series.mapValues(fun c -> 
                        c:?> bool |> not :> Comp)
            | AST.And(e1,e2) ->
                let exp1 = compileBooleanExpression e1
                let exp2 = compileBooleanExpression e2
                binaryOp (fun exp1 exp2 -> exp1 :?> bool && exp2 :?> bool) exp1 exp2
            | AST.Or(e1,e2) ->
                let exp1 = compileBooleanExpression e1
                let exp2 = compileBooleanExpression e2
                binaryOp (fun exp1 exp2 -> exp1 :?> bool || exp2 :?> bool) exp1 exp2
            | AST.Comparison(lhs,rhs,op) ->
                let opExp : System.IComparable -> System.IComparable -> bool =                
                    match op with
                    AST.GreaterThan -> (>)        
                    | AST.GreaterThanOrEqual -> (>=) 
                    | AST.LessThan -> (<)           
                    | AST.LessThanOrEqual -> (<=)    
                    | AST.EqualTo -> 
                         fun a b ->
                            match a,b with
                            null,null -> true
                            | null,_ | _,null -> false 
                            | _ -> a = b
                    | AST.Contains ->
                        fun lhs rhs ->
                            let lhsString = lhs |> string
                            let rhsString = rhs |> string
                            if String.IsNullOrEmpty(lhsString) then false
                            elif String.IsNullOrEmpty(rhsString) then true
                            else
                               lhsString.IndexOf(rhsString, StringComparison.CurrentCultureIgnoreCase) >= 0
                let lhsExp = compileExpression frame lhs
                let rhsExp = compileExpression frame rhs
                binaryOp opExp lhsExp rhsExp
            | AST.ValueOfColumn c ->
                fun _ ->
                    frame
                    |> Frame.getCol c
                    |> Series.mapValues(fun c -> c :> Comp)
                    
        let column columnExpression =
            match columnExpression with
            | AST.RenameColumn(orgColumnName, newColumnName) ->
                frame
                |> Frame.mapColKeys(fun columnName -> 
                    if columnName = orgColumnName then newColumnName else columnName
                )
            | AST.CreateColumn (exp, nameOfNewColumn) -> 
                try
                    let compiledExpression = compileExpression frame exp
                    let resultingSeries = compiledExpression (keySeries)
                    let newframe = Frame([nameOfNewColumn],[resultingSeries])
                    let result = 
                        newframe
                        |> Frame.join JoinKind.Outer frame
                    let col =
                        result.GetColumn nameOfNewColumn
                        |> Series.observationsAll
                        |> List.ofSeq
                    result
                with e ->
                    let cols = frame.ColumnKeys
                    eprintfn "%s. Columns: %A" e.Message cols
                    reraise()
            | AST.Pivot(rowKeyExpression,columnKeyExpression,valueExpression, reduction) ->
                let rowkey,compiledExpressionFunc = compileTempColumn "__rowkey__" rowKeyExpression (fun r ->
                                                                                                       r  |> AST.KeyType.OfOption
                                                                                                       :> Comp)
                compiledExpressionFunc frame keySeries
                let columnkey,compiledExpressionFunc = compileTempColumn "__columnkey__" columnKeyExpression (fun r ->
                                                                                                        let key = 
                                                                                                            match r with
                                                                                                            None -> ""
                                                                                                            | Some v -> 
                                                                                                                v |> string
                                                                                                        key :> Comp)
                compiledExpressionFunc frame keySeries
                let col = frame.GetColumn columnkey |> Series.observationsAll |> List.ofSeq
                let row = frame.GetColumn rowkey |> Series.observationsAll |> List.ofSeq
                
                assert(col <> row)

                let resultingFrame : Frame<AST.KeyType, string>  = 
                    frame
                    |> Frame.pivotTable 
                        (fun _ r -> 
                            r.GetAs<AST.KeyType> rowkey
                        )
                        (fun _ r -> 
                            r.GetAs<string> columnkey
                        )
                        (fun f ->
                              let resultsColumn,compiledExpressionFunc = compileTempColumn "__result__" valueExpression (fun v -> (v |> OptionalValue.ofOption).ValueOrDefault)
                              compiledExpressionFunc f keySeries
                              f.GetColumn resultsColumn
                              |>(match reduction with
                                 AST.Count -> 
                                      Series.countValues >> float
                                 | AST.Sum -> 
                                      Stats.sum
                                 | AST.Median -> 
                                      Stats.median
                                 | AST.Mean -> 
                                      Stats.mean
                                 | AST.StdDev -> 
                                      Stats.stdDev
                                 | AST.Variance -> 
                                      Stats.variance
                                 | AST.Max -> 
                                      Stats.max
                                 | AST.Min -> 
                                      Stats.min 
                                 
                                 ))

                match rowKeyExpression with
                AST.ComputationExpression.ColumnName name ->
                    resultingFrame?(name) <- 
                        resultingFrame.RowKeys
                        |> Seq.map AST.KeyType.UnWrap
                        |> Seq.cast<Comp>
                    resultingFrame
                | _ -> 
                    resultingFrame
        let reduce reduction = 
            let aggregate f = 
                frame
                |> Frame.getNumericCols
                |> Series.observations
                |> Seq.map(fun (k, v) -> 
                    k, [AST.KeyType.Create 0 => (f v :> Comp) ] |> series
                ) |> Frame.ofColumns
                |> DataMatrix
                :> IDataMatrix

            match reduction with
            AST.Sum -> 
                aggregate Stats.sum
            | AST.Count -> 
                aggregate Stats.count
            | AST.Median -> 
                aggregate Stats.median
            | AST.Mean -> 
                aggregate Stats.mean
            | AST.StdDev -> 
                aggregate Stats.stdDev
            | AST.Variance -> 
                aggregate Stats.variance
            | AST.Max -> 
                aggregate Stats.max
            | AST.Min -> 
                aggregate Stats.min 
        
        let filterAndSort filter =
            let f = 
                match filter with
                AST.SliceColumns cols ->
                    fun frame ->
                        let res = 
                            frame
                            |> Frame.sliceCols cols
                        //There might be fewer columns, than in the slice command but no more

                        assert(if res.ColumnCount <= (cols |> Seq.length) then 
                                    true 
                               else 
                                   printfn "%A <> %A" (System.String.Join(",",res.ColumnKeys)) (System.String.Join(",",cols))
                                   false
                        )
                        res
                | AST.IndexBy exp ->
                    fun frame ->
                        let columnName = 
                            match exp with
                            AST.ColumnName name ->
                                //Lets use the optimized version if it's a column and not a calculated expression
                                name
                            | _ -> 
                                let indexByColumnName ="__index__"
                                let exp = compileExpression frame exp
                                frame?(indexByColumnName) <- 
                                    (exp keySeries)
                                indexByColumnName
                        let resultingFrame = 
                            frame
                            |> Frame.indexRows columnName
                            |> Frame.mapRowKeys(AST.KeyType.Create)
                           
                        (match exp with
                         AST.ColumnName name ->
                             resultingFrame?(name) <-
                                 resultingFrame.RowKeys
                                 |> Seq.map AST.KeyType.UnWrap
                                 |> Seq.cast<Comp>
                             resultingFrame
                         | _ -> 
                             resultingFrame)
                        |> Frame.sortRowsByKey

                | AST.SortBy columnName ->
                     Frame.sortRows columnName
                | AST.DenseRows->
                     Frame.denseRows
                     >> Frame.ofRows
                | AST.DenseColumns ->
                     Frame.denseCols
                     >> Frame.ofColumns
                | AST.NumericColumns->
                     Frame.getNumericCols
                     >> Frame.ofColumns
                | AST.Only condition ->
                    fun frame ->
                        let conditionColumn = "__condition__"
                        frame?(conditionColumn) <- compileBooleanExpression condition keySeries
                        frame
                        |> Frame.filterRows(fun _ row ->
                            (row.TryGetAs<bool> conditionColumn).ValueOrDefault
                        )
                        |> Frame.dropCol conditionColumn
                
            frame |> f
        let cluster c  =
            let f = 
                match c with
                 | AST.Buckets(b,reduction) -> Clustering.counting b reduction 
                 | AST.KMeansClustering (b,reduction) -> Clustering.kmeans b reduction 
                 | AST.Each reduction ->
                        Clustering.each reduction 
                 | AST.GroupBy(columnNames, groupReduction) ->
                     let reducer = 
                         match groupReduction with
                         | AST.Reduce reduction -> (Clustering.reduceGroup reduction)
                         | AST.Select selector ->
                             let select expression f _ grouped = 
                                    let selectorKey, compiledExpressionFunc = compileTempColumn "__selector__" expression (fun r ->
                                                                                                                               r  |> AST.KeyType.OfOption
                                                                                                                               :> Comp)
                                    let cols = 
                                        grouped
                                        |> Frame.getCols
                                        |> Series.observations
                                        |> Seq.map fst
                                    let values = 
                                        grouped
                                        |> Frame.nest
                                        |> Series.observations
                                        |> Seq.map (fun (_, subFrame) -> 
                                            let keySeries = 
                                                subFrame
                                                |> Frame.getCols
                                                |> Series.observations
                                                |> (Seq.head >> snd)
                                                |> Series.map(fun k _ -> (k |> AST.KeyType.UnWrap) :?> Comp)
                                            compiledExpressionFunc subFrame keySeries
                                            let rowKey,_ = 
                                                  subFrame.GetColumn selectorKey
                                                  |> Series.observations
                                                  |> f snd
                                            rowKey, subFrame |> Frame.getRow rowKey
                                        ) |> Frame.ofRows
                                    //remove the selector column    
                                    values
                                    |> Frame.sliceCols cols
                             match selector with
                             AST.MaxBy expression ->
                                 select expression (Seq.maxBy)
                             | AST.MinBy expression ->
                                 select expression (Seq.minBy)
                     Clustering.group columnNames reducer 
            f frame

        let toTable frame = 
            frame
            |> Frame.cols
            |> Series.observations
            |> Seq.map(fun (columnName, values) -> 
                columnName, 
                values 
                |> Series.observationsAll
                |> Seq.map(fun (k,v) -> 
                    k, 
                    match v with
                    None -> null
                    | Some v -> v
                )

            ) |> Seq.filter(fun (_,values) -> values |> Seq.isEmpty |> not)
        
        let encodeValue (value : obj) = 
            match value with
            null -> Encode.nil
            | :? DateTime as d -> 
                d.ToString() |> Encode.string
            | :? DateTimeOffset as d -> 
                d.ToLocalTime().DateTime.ToString() |> Encode.string
            | :? int as i  -> i |> Encode.int
            | :? float as f -> f |> Encode.float
            | :? decimal as d -> d |> Encode.decimal
            | :? string as s  -> s |> Encode.string
            | :? bool as b -> b |> Encode.bool
            | _ -> failwithf "Don't know how to encode %A" value

        member private ___.Columns 
            with get() =
                frame
                |> Frame.cols
                |> Series.observations
                |> Seq.map(fun (columnName,values) ->
                    columnName, values 
                                |> Series.observationsAll
                                |> Seq.map(function
                                             k,None -> k,null
                                             | k,Some s -> k,s) 
                )
        member private ___.Frame with get() = frame
        
        member ___.AsTable() =
                frame
                |> Frame.cols
                |> Series.observations
                |> Seq.map(fun (columnName,values) ->
                    columnName,
                    values
                    |> Series.observations
                    |> Seq.map(fun (key,v) -> key,v :?> IComparable)
                )

        interface IDataMatrix with
            member ___.RowCount = frame.RowCount
            member ___.Join fieldName other = 
                frame
                |> Frame.indexRows fieldName
                |> Frame.join JoinKind.Outer <| 
                    (other :?> DataMatrix).Frame
                    |> Frame.indexRows fieldName
                |> DataMatrix
                :> IDataMatrix
            member ___.Combine other =
                let matrix1 = 
                    frame |> toTable
                let matrix2 =
                    (other :?> DataMatrix).Frame
                    |> toTable

                matrix1 |> Seq.fold(fun m (columnName,values) ->
                    match m |> Map.tryFind columnName with
                    None -> m.Add(columnName, values)
                    | Some vs -> m.Add(columnName,vs |> Seq.append values)
                ) (matrix2 |> Map.ofSeq)
                |> Map.toSeq
                |> Seq.map(fun (columnName,values) -> columnName, values |> series)
                |> series
                |> Frame.ofColumns
                |> DataMatrix
                :> IDataMatrix

            member this.Transform expr =
                match expr with
                AST.Reduction reduction -> 
                    reduce reduction
                | AST.FilterAndSorting fs ->
                    filterAndSort fs
                    |> DataMatrix
                    :> IDataMatrix
                | AST.Cluster c ->
                    cluster c
                    |> DataMatrix
                    :> IDataMatrix
                | AST.Column c ->
                    column c
                    |> DataMatrix
                    :> IDataMatrix
            member this.ToJson() =
                let table = 
                    frame
                    |> toTable

                let columnNames = 
                    table |> Seq.map fst |> Array.ofSeq

                let values =
                    table
                    |> Seq.map(fun (_,values) ->
                        values
                        |> Seq.map (fun (_,v) -> v) 
                        |> Array.ofSeq
                    ) |> Array.ofSeq
                    |> Array.transpose
   
                
                Encode.array(
                    values
                    |> Array.map(
                        Array.mapi(fun i (cell : obj) ->
                            let encodedValue = 
                                match cell with
                                null -> Encode.nil
                                | :? System.DateTime as d -> 
                                    d.ToString() |> Encode.string
                                | :? System.DateTimeOffset as d -> 
                                    d.ToLocalTime().DateTime.ToString() |> Encode.string
                                | :? int as i  -> i |> Encode.int
                                | :? float as f -> f |> Encode.float
                                | :? decimal as d -> d |> Encode.decimal
                                | :? string as s  -> s |> Encode.string
                                | :? bool as b -> b |> Encode.bool
                                | _ -> failwithf "Don't know how to encode %A" cell
                            columnNames.[i],encodedValue
                        ) >> List.ofArray 
                        >> Encode.object
                    )
                ) 
                

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DataMatrix =
        let empty = DataMatrix(Frame([],[]))
        let fromRows rows  = 
            let frame = 
                rows
                |> Seq.map(fun (rowKey, values) ->
                    AST.KeyType.Create rowKey =>
                     (values |> series)
                ) |> series
                |> Frame.ofRows
            frame
            |> DataMatrix
            :> IDataMatrix
        
        let fromTable table =
                let frame = 
                    table
                    |> Seq.map(fun (columnName, values) ->
                        columnName,
                        values |> series
                    ) |> series
                    |> Frame.ofColumns
                frame
                |> DataMatrix
                :> IDataMatrix
        let toJson (matrix : #IDataMatrix) =
            matrix.ToJson()
        