namespace Hobbes.Parsing

open FParsec
open AST
open Hobbes.Parsing.Primitives

module Expressions = 
    
    let checkBooleanExp  = 
             function 
                 ColumnName c -> ValueOfColumn c
                 | ComputationExpression.Boolean b -> b
                 | _ -> 
                     //TODO: rewrite to user the FParsec error reporting
                     failwith "Expected a boolean expression"
    

    let formatDate : Parser<_> = 
        pipe2 (kwFormat .>>? kwDate >>. spaces1 >>. columnName .>> spaces1) dateFormat (fun columnName format -> AST.FormatDate(columnName,format))
    

    let private opp  = new OperatorPrecedenceParser<AST.ComputationExpression,unit,YamlParser.Types.State>()
    let private expr : Parser<_> = opp.ExpressionParser
    
    let expressionInBrackets : Parser<_> = 
        spaces >>. (between (skipString "[" .>> spaces) (spaces >>. skipString "]") expr) .>> spaces

    let private computationExpression =
        // we set up an operator precedence parser for parsing the arithmetic expressions
        
        //moving mean 3 [expr]
        let moving = 
            kwMoving >>? (pipe3 (reduction)  (spaces1 >>. pint32) expressionInBrackets (fun reduction windowSize columnExpression ->
               AST.Moving(reduction, windowSize, columnExpression) |> ColumnExpression
            ))

        //expanding sum [expr]
        let expanding = 
            pipe2  (kwExpanding >>? reduction) expressionInBrackets (fun reduction columnExpression ->
                AST.Expanding(reduction,columnExpression) |> ColumnExpression
            )
        let ordinals = kwOrdinals >>= (fun _ -> Ordinals |> preturn)
        let regression = 
            pipe2 (kwLinear .>>? 
                   kwRegression >>.
                   expressionInBrackets)
                  expressionInBrackets
                  (fun inputs outputs -> Regression(Linear,inputs,outputs)) 

        let extrapolation = 
            pipe3 (kwLinear .>>? kwExtrapolation >>.
                expressionInBrackets)
                (pint32)
                ((spaces1 >>? pint32 >>= (Some >> preturn)) <|> (spaces >>= (fun _ -> None |> preturn)))
                (fun outputs count trainingLength -> Extrapolate(Linear,outputs, count, trainingLength)) 

        let ``int`` = kwInt >>? expr >>= (AST.Int >> preturn) 

        let ifThisThenElse =
            let expressionInCurly = spaces >>. (between (skipString "{" .>> spaces) (spaces >>. skipString "}") expr) .>> spaces
            (pipe3 (kwIf >>? expressionInBrackets)
                  (expressionInCurly)
                  (kwElse >>? expressionInCurly)
                  (fun condition thenBody elseBody -> 
                      let condition = 
                          checkBooleanExp condition
                      AST.IfThisThenElse(condition,thenBody,elseBody)
                  ))

        let regExGroupExpression : Parser<_> = 
            (pstring "$" >>. pint32) >>= (AST.RegExGroupIdentifier >> preturn)
            <|> (pquotedStringLiteral >>= (AST.RegExResultString >> preturn))

        let regExGroupExpressions : Parser<_> =
            let expr : Parser<_> = sepBy regExGroupExpression (skipString "+")
            between (skipString "[") (skipString "]") expr

        let regex = 
            pipe3 (kwRegex >>. expressionInBrackets) regexLiteral regExGroupExpressions (fun expr literal result ->
               RegularExpression(expr, literal.Replace("\\/","/"),result) 
            )
        let number : Parser<_> = (pnumber >>= (AST.Number >> preturn))
        opp.TermParser <- 
            ifThisThenElse
            <|> int
            <|> moving
            <|> expanding
            <|> regression
            <|> regex
            <|> extrapolation
            <|> number
            <|> kwMissing
            <|> kwKeys
            <|> formatDate
            <|> quotedStringLiteral .>> spaces
            <|> (columnName .>> spaces >>= (AST.ColumnName >> preturn))
            <|> between (stringThenWhiteSpace "(") (stringThenWhiteSpace ")") expr
            

        // operator definitions follow the schema
        // operator type, string, trailing whitespace parser, precedence, associativity, function to apply
        let exp op lhs rhs = 
            AST.Binary(lhs,rhs,op)
        let comp op lhs rhs = 
            AST.Comparison(lhs,rhs,op) |> AST.Boolean
        
        let andOr op lhs rhs = 
            let l = checkBooleanExp lhs
            let r = checkBooleanExp rhs
            op(l,r) |> AST.Boolean
        ((PrefixOperator("!", spaces, 5, true, (fun e -> 
              let exp = checkBooleanExp e
              exp 
              |> AST.Not
              |> AST.Boolean)
         ) :> Operator<_,_,_>)
        :: [
                InfixOperator("+", spaces, 3, Associativity.Left, exp AST.Addition)
                InfixOperator("-", spaces, 3, Associativity.Left, exp AST.Subtraction)
                InfixOperator("*", spaces, 4, Associativity.Left, exp AST.Multiplication)
                InfixOperator("/", spaces, 4, Associativity.Left, exp AST.Division)
                InfixOperator("%", spaces, 4, Associativity.Left, exp AST.Modulo)

                InfixOperator("&&", spaces, 1, Associativity.Left, (andOr AST.And))
                InfixOperator("||", spaces, 1, Associativity.Left, (andOr AST.Or))

                InfixOperator("contains", spaces1, 1, Associativity.Left, comp AST.Contains)
                InfixOperator(">",  spaces, 2, Associativity.Left, comp AST.GreaterThan)
                InfixOperator(">=", spaces, 2, Associativity.Left, comp AST.GreaterThanOrEqual)
                InfixOperator("<=", spaces, 2, Associativity.Left, comp AST.LessThanOrEqual)
                InfixOperator("<",  spaces, 2, Associativity.Left, comp AST.LessThan)
                InfixOperator("=",  spaces, 2, Associativity.Left, comp AST.EqualTo)
                
            ]) |> List.iter(opp.AddOperator)
                                                             
        //opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, fun x y -> System.Math.Pow(x, y)))
        //opp.AddOperator(PrefixOperator("-", ws, 4, true, fun x -> -x))
        spaces >>. expr
        
        
    let expression =
        computationExpression