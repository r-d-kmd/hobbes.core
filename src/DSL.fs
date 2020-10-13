module Hobbes.DSL

open Hobbes.Parsing

let private culture = System.Globalization.CultureInfo.CurrentCulture

let toString = 
    function
      | AST.Count -> "count"
      | AST.Sum -> "sum"
      | AST.Median -> "median"
      | AST.Mean -> "mean"
      | AST.StdDev -> "stddev"
      | AST.Variance -> "variance"
      | AST.Max -> "max"
      | AST.Min -> "min"

type Expression = 
    Identifier of string
    | TextLiteral of string
    | Expanding of AST.Reduction  * Expression
    | Moving of AST.Reduction * windowSize : int * Expression
    | Subtraction of Expression * Expression
    | Equal of Expression * Expression
    | If of Expression * Expression * Expression
    | Or of Expression * Expression
    | And of Expression * Expression
    | Gt of Expression * Expression
    | NumberConstant of float
    | DateTimeConstant of System.DateTime
    | DateFormat of string * AST.DateFormat
    | Not of Expression
    | Regression of AST.Regression * Expression * Expression
    | Extrapolation of AST.Regression * Expression * int * int option
    | RegularExpression of input : Expression * patter: string * resultSnippets : AST.RegExResultToken list
    | Ordinals
    | Missing
    | Keys
    | Int of Expression
    | True
    | False
    with override x.ToString() =
           match x with
             Identifier s -> sprintf """ "%s" """ s
             | RegularExpression(input, pattern, results) ->
                let tokens = 
                    (System.String.Join(" ", 
                        results 
                        |> List.map(function 
                                        AST.RegExGroupIdentifier n -> sprintf "$%d" n 
                                        | AST.RegExResultString s -> sprintf "'%s'" s)
                                   )
                    )
                sprintf "regex [%s] /%s/ [%s]" (input.ToString()) pattern tokens
             | Expanding (r,exp) ->
                 sprintf "expanding %s [%s]"  (toString(r)) (exp.ToString())
             | Missing -> "missing"
             | Moving (r,windowSize, exp) ->
                 sprintf "moving %s %d [%s]"  (toString(r)) windowSize (exp.ToString())
             | Subtraction(a,b) -> sprintf " %s - %s" (a.ToString()) (b.ToString())
             | TextLiteral s -> sprintf " '%s' " s
             | Equal(a,b) -> sprintf " %s = %s" (a.ToString()) (b.ToString())
             | If(condition, thenBody, elseBody) ->
                 sprintf " if [%s] {%s} else {%s}" (condition.ToString()) (thenBody.ToString()) (elseBody.ToString())
             | Or(a,b) -> sprintf " (%s) || (%s) "  (a.ToString()) (b.ToString())
             | And(a,b) -> sprintf " (%s) && (%s) "  (a.ToString()) (b.ToString())
             | NumberConstant i -> i |> string
             | Gt(a,b) ->  sprintf " (%s) > (%s)" (a.ToString()) (b.ToString())
             | Not e -> sprintf "!(%s)" (e.ToString())
             | Keys -> "keys"
             | DateTimeConstant d -> sprintf "\'%s\'" (d.ToString(System.Globalization.CultureInfo.InvariantCulture))
             | Ordinals -> "ordinals"
             | DateFormat(columnName,f) ->
                 let dateFormat = 
                     match f with
                     AST.Year -> "year"
                     | AST.Month -> "month"
                     | AST.Day -> "day"
                     | AST.Date -> "date"
                     | AST.Week -> "week"
                     | AST.Weekday -> "weekday"
                 sprintf """format date "%s" %s """ columnName dateFormat
             | Regression(reg, inputs, outputs) ->
                 let regStr = 
                     match reg with
                     AST.Linear -> "linear"
                 sprintf "%s regression [%s] [%s]" regStr (inputs.ToString()) (outputs.ToString())
             | Extrapolation(reg, outputs, count, length) ->
                 let regStr = 
                     match reg with
                     AST.Linear -> "linear"
                 let l = 
                    match length with
                    None -> ""
                    | Some l -> sprintf " %d" l
                 sprintf "%s extrapolation [%s] %d %s" regStr (outputs.ToString()) count l
             | Int e -> sprintf "int (%s)" (e.ToString())
             | True -> "1 = 1"
             | False -> "1 = 2"
           
         static member private ParseStringOrDate (stringOrDate : string) = 
            match System.DateTime.TryParse(stringOrDate) with
            true, v  -> DateTimeConstant v
            | false, _ -> TextLiteral stringOrDate
         static member (-) (e1:Expression, e2:Expression) = 
             Subtraction(e1,e2)
         static member (==) (e1:Expression, e2:string) = 
             let e2 = Expression.ParseStringOrDate e2
             e1 == e2
         static member (==) (e1:Expression, e2:Expression) = 
             Equal(e1,e2)
         static member (==) (e1:Expression, e2:int) = 
             e1 == (e2 |> float |> NumberConstant)
         static member (==) (e1:Expression, e2:System.DateTime) = 
             e1 == (e2 |> DateTimeConstant)
         
         static member (!=) (e1:Expression, e2:Expression) = 
             Not(Equal(e1,e2))     
         static member (!=) (e1:Expression, e2:string) = 
             let e2 = Expression.ParseStringOrDate e2
             e1 != e2
             
         static member (.||) (exp1:Expression,exp2:Expression) =
             Or(exp1,exp2)
         static member (.&&) (exp1:Expression,exp2:Expression) =
             And(exp1,exp2)
         static member (.>) (exp1:Expression,exp2:Expression) =
             Gt(exp1,exp2)
         static member (.>) (exp1:Expression,exp2:int) =
             Gt(exp1,exp2 |> float |> NumberConstant)
         static member (.>) (exp1:int,exp2:Expression) =
             Gt(exp1 |> float |> NumberConstant, exp2)
         
type Selector = 
    MaxBy of Expression
    | MinBy of Expression
    with override x.ToString() =
            let s,e = 
                match x with
                MaxBy e -> "maxby",e
                | MinBy e -> "minby",e 
            sprintf "%s %s" s (e.ToString())

type  Grouping = 
    Simple of columnNames: string list * reduction : AST.Reduction
    | RowSelection of columnNames: string list * selector : Selector

type ColumnsOrRows =
     Rows
     | Columns
type Statements = 
    GroupStatement of Grouping
    | CreateColumn of name:string * expression:Expression
    | Rename of string * string
    | Pivot of Expression * Expression * AST.Reduction * Expression
    | Slice of ColumnsOrRows * string list
    | Dense of ColumnsOrRows
    | Only of Expression
    | Sort of string
    | Index of Expression
    with override x.ToString() = 
           match x with
           GroupStatement grp ->
               let formatExpressions expressions = 
                      let expressions = 
                          System.String.Join(" ", 
                              expressions
                              |> List.map string)
                      sprintf "group by %s -> %s" expressions
               match grp with
               Simple (columns,reduction) ->
                  columns |> formatExpressions <| (reduction |> toString)
               | RowSelection(columns, selector)  ->
                  let grp = columns |> formatExpressions
                  let sel = selector.ToString()
                  grp sel
           | CreateColumn(name,exp) ->
              sprintf """create column "%s" (%s)""" name (exp.ToString())
           | Rename(orgColumn,newColumn) ->
                sprintf """rename column "%s" "%s" """ orgColumn newColumn
           | Pivot(exp1,exp2,r, exp3) ->
              sprintf "pivot [%s] [%s] -> %s [%s]" (exp1.ToString()) (exp2.ToString()) (r |> toString) ((exp3.ToString()))
           | Slice(Rows,rows) -> 
               System.String.Join(" ", rows |> List.map (sprintf """ "%s" """))
               |> sprintf "slice rows %s"
           | Slice(Columns,columns) -> 
               System.String.Join(" ", columns |> List.map (sprintf """ "%s" """))
               |> sprintf "slice columns %s"
           | Dense(Rows) -> "dense rows"
           | Dense(Columns) -> "dense columns"
           | Sort(name) -> sprintf """sort by column "%s" """ name
           | Index(exp) -> sprintf """index rows by (%s) """ (exp.ToString())
           | Only exp -> sprintf "only (%s)" (exp.ToString())

let by = ()


let inline (!!>) (text:string) = 
         TextLiteral text

let inline (!>) (identifier:string) = 
         Identifier identifier

let inline (<!>) (regexLiteral : string) =
    AST.RegExResultString regexLiteral

type GroupBy = 
    GroupByWithExpressions of string list
    with static member (=>) (grouping:GroupBy,r : AST.Reduction) = 
             Simple(grouping.Expressions, r)  |> GroupStatement
         static member (=>) (grouping:GroupBy,r : Selector) = 
             RowSelection(grouping.Expressions, r) |> GroupStatement
         member grouping.Expressions 
             with get() =
                 match grouping with GroupByWithExpressions es -> es

let group _ expressions = 
    GroupByWithExpressions expressions

let expanding reduction expression = 
    Expanding(reduction,expression)

let moving reduction windowSize expression = 
    Moving(reduction,windowSize,expression)

let expandingFloat reduction expression =
    Expanding(reduction,NumberConstant expression)

let movingFloat reduction windowSize expression = 
    Moving(reduction,windowSize,NumberConstant expression)

let expandingInt reduction (expression : int) = 
    float expression |> expandingFloat reduction 

let movingInt reduction windowSize (expression :int) = 
    float expression |> movingFloat reduction windowSize

let expandingStr reduction expression = 
    Expanding(reduction,Identifier expression)

let movingStr reduction windowSize expression = 
    Moving(reduction,windowSize,Identifier expression)

let maxby expression =
    MaxBy expression

let minby exp = MinBy exp

let column name = name
let create column exp = CreateColumn(column, exp)
let rename orgColumn newColumn = Rename(orgColumn,newColumn)
let pivot exp1 exp2 reduction exp3 =
    Pivot(exp1,exp2,reduction, exp3)
let columns = Columns
type Else(expression: Expression) = 
    member x.Expression with get() = expression
    new(number: int) = 
        Else(number |> float |> NumberConstant)
    new(number: float) = 
        Else(number |> NumberConstant)
    new(literal : string) = 
        Else(TextLiteral(literal))
type Then = Else
let If condition (thenBody : Else) (elseBody : Else) =
   If(condition, thenBody.Expression, elseBody.Expression)

let inline contains (expression : Expression) (expressions : Expression list)  =
    match expressions with
    [] -> Equal(NumberConstant 1., NumberConstant 2.) //the empty set can't contain anything and we don't have a false literal
    | e::tail ->
       Equal(expression,e)::tail
       |> List.reduce(fun s e -> Or(s,Equal(expression,e)))

let rows = Rows
let dense = Dense
let slice colOrRow columnNames = 
    Slice(colOrRow,columnNames)
let sort _ name = 
    Sort(name)
let index _ _ exp =
    Index(exp)

let only expression = 
    Only(expression)

let ordinals = Ordinals

let linear f = f AST.Linear
let regression regressionType inputs outputs = 
    Regression(regressionType,inputs,outputs)

let extrapolation regressionType outputs count= 
    Extrapolation(regressionType,outputs,count, None)

let extrapolationLimited regressionType outputs count length= 
    Extrapolation(regressionType,outputs,count, Some length)


let ``$1`` = AST.RegExGroupIdentifier 1
let ``$2`` = AST.RegExGroupIdentifier 2
let ``$3`` = AST.RegExGroupIdentifier 3
let ``$4`` = AST.RegExGroupIdentifier 4

let regex expr pattern tokens =
    RegularExpression(expr, pattern, tokens)

let int e = Int(e)

let isMissing (e : Expression) = e == Missing
let isntMissing (e : Expression) = e != Missing 
let format = ()
let date _ columnName dt =
    DateFormat(columnName,dt)