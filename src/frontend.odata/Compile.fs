namespace Hobbes.OData

open Hobbes.Parsing

module Compile = 
    type Filter =
        Eq of string * string
        | Gt of string * string
        | Lt of string * string
        | Ge of string * string
        | Le of string * string
        | Ne of string * string
        | Contains of string * string
        | And of Filter * Filter
        | Or of Filter * Filter
        | Not of Filter
        override x.ToString() =
            let binary op lhs rhs =
                sprintf "( %s %s %s )" lhs op rhs
            match x with
            Eq (lhs,rhs) -> binary "eq" lhs rhs
            | Gt (lhs,rhs) -> binary "gt" lhs rhs
            | Lt (lhs,rhs) -> binary "lt" lhs rhs
            | Ge (lhs,rhs) -> binary "ge" lhs rhs
            | Le (lhs,rhs) -> binary "le" lhs rhs
            | Ne (lhs,rhs) -> binary "ne" lhs rhs
            | Contains(lhs,rhs) -> sprintf "(contains(%s,%s))" lhs rhs
            | And (lhs,rhs) -> binary "and" (string lhs) (string rhs)
            | Or (lhs,rhs) -> binary "" (string lhs) (string rhs)
            | Not f -> 
                f 
                |> string 
                |> sprintf "(not %s)"

        
    type CompileResult = {
        Filters : Filter list
        Fields : string list
        OrderBy : string list
    }
           
    let parsedExpressions (expressions : seq<AST.Statement>) = 
        expressions

    let rec compileComparisonExpression lhs rhs op  =
        let compiledLhs = compileExpression lhs
        let compiledRhs = compileExpression rhs

        let ops = 
            match op with
            AST.GreaterThan -> Gt        
            | AST.GreaterThanOrEqual -> Ge
            | AST.LessThan -> Lt          
            | AST.LessThanOrEqual -> Le
            | AST.EqualTo -> Eq
            | AST.Contains -> Contains
        ops (compiledLhs,compiledRhs)

    and compileExpression exp =
        let binary lhs rhs op = 
            let compiledLhs = compileExpression lhs
            let compiledRhs = compileExpression rhs
            let simpleBinary op lhs rhs =
                sprintf "%s %s %s" lhs op rhs

            let ops =
                match op with
                AST.Addition -> 
                    match lhs with 
                    AST.String _ -> 
                        sprintf "concat(%s,%s)"
                    | _ -> simpleBinary "add"         
                | AST.Subtraction -> simpleBinary "sub"   
                | AST.Multiplication -> simpleBinary "mul"
                | AST.Division -> simpleBinary "div"
                | AST.Modulo -> simpleBinary "mod"
            ops compiledLhs compiledRhs
        
        let exp = 
            match exp with
            AST.Binary(lhs,rhs,op) -> binary lhs rhs op
            | AST.Boolean b -> b |> string
            | AST.Number n -> 
                  match n with
                  AST.Int32 n -> string n
                  | AST.Int64 n -> string n
                  | AST.Float n -> string n
            | AST.Int(AST.Number n) -> 
                match n with
                  AST.Int32 n -> int n
                  | AST.Int64 n -> int n
                  | AST.Float n -> int n
                |> string
            | AST.Int(AST.String s) ->
                s |> int |> string
            | AST.MissingValue -> "null"
            | AST.String s -> sprintf "'%s'" s
            | AST.DateTime dt -> dt.ToString()
            | AST.ColumnName cn -> cn
            | AST.FormatDate (cn,format) ->
                let formatDate op column =
                    sprintf "%s(%s)" op column
                let func = 
                    match format with
                    AST.Year -> "year"
                    | AST.Month -> "month"
                    | AST.Day -> "day"
                    | AST.Date -> "date"
                    | AST.Week 
                    | AST.Weekday -> failwith "not supported"
                formatDate func cn
            | AST.Int _
            | AST.Keys 
            | AST.ColumnExpression _ 
            | AST.Regression _ 
            | AST.Extrapolate _
            | AST.IfThisThenElse _
            | AST.Ordinals -> failwith "Not supported for OData"
            | AST.RegularExpression _ -> failwith "Not implemented"
        sprintf "(%s)" exp

    and compileBooleanExpression c =
        let compiledExp = 
            match c with 
            AST.And (lhs,rhs) ->
                And(compileBooleanExpression lhs,compileBooleanExpression rhs)
            |AST.Or(lhs,rhs) ->
                Or(compileBooleanExpression lhs, compileBooleanExpression rhs)
            |AST.Not(AST.Comparison(lhs,rhs,AST.EqualTo)) ->
                Ne(compileExpression lhs,compileExpression rhs)
            |AST.Not(exp) ->
                exp |> compileBooleanExpression |> Not
            |AST.Comparison(lhs,rhs,op) ->
                compileComparisonExpression lhs rhs op
            |AST.ValueOfColumn fieldName -> 
                Eq(fieldName,"true")
        compiledExp

    let expressions (statemens : string) = 
        let rec union list1 list2 =
            match list1, list2 with
            | [], other | other, [] -> other
            | x::xs, y::_ when x < y -> x :: (union xs list2)
            | _, y::ys -> y :: (union list1 ys)

        match statemens.Trim() with
        "" -> 
            {
                Fields = []
                Filters = []
                OrderBy = []
            }
        | lines ->
            let ast = StatementParser.parse statemens
            
            let fields,orderBy = 
               ast 
               |> Seq.fold(fun (fields,orderBy) a ->
                  match a with
                  AST.FilterAndSorting fs ->
                      match fs with
                      AST.Only _ ->  (fields,orderBy)
                      | AST.SliceColumns a ->
                            (fields |> List.sort |> union a,orderBy)
                      | AST.DenseColumns
                      | AST.DenseRows  
                      | AST.NumericColumns
                      | AST.IndexBy _ -> failwith "Not supported"
                      | AST.SortBy name ->  
                          (fields, name::orderBy)
                  | AST.Reduction _
                  | AST.Cluster _ 
                  | AST.Column _ -> failwith "Not implemented"
               )([],[])
            let filters =
                ast
                |> Seq.fold(fun filters a ->
                    match a with
                    AST.FilterAndSorting fs ->
                        match fs with
                        AST.Only(c) ->
                            (compileBooleanExpression c)::filters
                        | AST.SliceColumns _ -> filters
                        | AST.DenseColumns
                        | AST.DenseRows  
                        | AST.NumericColumns
                        | AST.IndexBy _ -> failwith "Not supported"
                        | AST.SortBy _ -> filters
                    | AST.Reduction _
                    | AST.Cluster _ 
                    | AST.Column _ -> failwith "Not implemented"
                ) []
          
            {
                Fields = fields
                Filters = filters
                OrderBy = orderBy
            }            