namespace Hobbes.Parsing

open FParsec.CharParsers
open FParsec.Primitives
open Hobbes.Parsing.Primitives

[<AutoOpen>]
module Keywords = 
    let private createOperator op : Parser<_> = 
        spaces >>. skipString op .>> spaces
    let opArrow  = createOperator "->" 
    let opDoubleArrow = createOperator "=>" 

    let private createKeyword word : Parser<_> = 
        skipString word .>> (spaces1 <|> eof)
    let private createKeywordReturn word ast  : Parser<_> =
        stringReturn word ast
    let internal kwBuckets = createKeyword "buckets"
    let internal kwBy  =       createKeyword "by"
    
    let internal kwColumn   =  createKeyword "column" 
    let internal kwColumns  =  createKeyword "columns"
    let internal kwCount  = createKeywordReturn "count" AST.Count
    let internal kwCreate   =  createKeyword "create"  
    let internal kwRename   =  createKeyword "rename" 

    let internal kwDate = createKeywordReturn "date" AST.Date
    let internal kwDay = createKeywordReturn "day" AST.Day
    let internal kwDense    =  createKeyword "dense"
    let internal kwEach = createKeyword "each" 

    let internal kwElse  =     createKeyword "else"
    let internal kwExpanding = createKeyword "expanding"
    let internal kwExtrapolation = createKeyword "extrapolation"  
    let internal kwFormat = createKeyword "format"

    let internal kwGroup = createKeyword "group" 

    let internal kwIf  =       createKeyword "if"
    let internal kwIndex    =  createKeyword "index"
    let internal kwInt  =      createKeyword "int" 

    let internal kwKeys  =    createKeywordReturn "keys" AST.Keys
    let internal kwKMeans = createKeyword "k-means"

    let internal kwLinear  =    createKeyword "linear"

    let internal kwMaxBy = createKeyword "maxby"
    let internal kwMax = createKeywordReturn "max"  AST.Max
    let internal kwMean = createKeywordReturn "mean" AST.Mean
    let internal kwMedian = createKeywordReturn "median" AST.Median 
    let internal kwMin = createKeywordReturn "min"  AST.Min
    let internal kwMinBy = createKeyword "minby"
    let internal kwMissing  =  createKeywordReturn "missing" AST.MissingValue
    let internal kwMonth = createKeywordReturn "month" AST.Month
    let internal kwMoving  = createKeyword "moving"

    let internal kwNumeric  =  createKeyword "numeric" 

    let internal kwOnly  =     createKeyword "only"
    let internal kwOrdinals  =     createKeyword "ordinals"
    
    let internal kwPivot  =    createKeyword "pivot"  
    let internal kwRegex  =     createKeyword "regex"
    let internal kwRegression  =    createKeyword "regression"  
    
    let internal kwRows     =  createKeyword "rows"

    let internal kwSlice    =  createKeyword "slice" 
    let internal kwSort     =  createKeyword "sort"
    let internal kwStdDev = createKeywordReturn "stddev" AST.StdDev
    let internal kwSum = createKeywordReturn "sum" AST.Sum

    let internal kwThen  =     createKeyword "then"

    let internal kwVariance = createKeywordReturn "variance" AST.Variance

    let internal kwWeek = createKeywordReturn "week" AST.Week
    let internal kwWeekDay = createKeywordReturn "weekday" AST.Weekday
    let internal kwYear = createKeywordReturn "year" AST.Year

    let reduction : Parser<_> = 
       [
           kwCount
           kwSum
           kwMean
           kwMedian
           kwStdDev
           kwVariance
           kwMax
           kwMin
       ]  |> List.reduce(<|>)

    let dateFormat = 
        [
            kwYear
            kwMonth
            kwDay
            kwDate
            kwWeek
            kwWeekDay
        ] |> List.reduce (<|>)