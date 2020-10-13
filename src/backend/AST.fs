namespace Hobbes.Parsing
open System.Reflection

module AST =
    
    type Number = Int32 of int32
                | Int64 of int64    
                | Float of float

    type RegExResultToken = 
        RegExGroupIdentifier of int
        | RegExResultString of string        
 
    type Reduction = 
        | Count
        | Sum
        | Median
        | Mean
        | StdDev
        | Variance
        | Max
        | Min

    type BinaryOperator = 
        Addition         
        | Subtraction    
        | Multiplication 
        | Division
        | Modulo
        
    type ComparisonOperator = 
          GreaterThan        
        | GreaterThanOrEqual 
        | LessThan           
        | LessThanOrEqual    
        | EqualTo    
        | Contains

    type DateFormat = 
        Year
        | Month
        | Day
        | Date
        | Week
        | Weekday
    type Selector = 
        MaxBy of ComputationExpression
        | MinBy of ComputationExpression
    and GroupReduction = 
        Select of Selector
        | Reduce of Reduction
    and Cluster = 
        Buckets of noOfbuckets: int * Reduction
        | KMeansClustering of noOfClusters: int * Reduction
        | Each of Reduction
        | GroupBy of columnNames: string list * GroupReduction: GroupReduction
    and BooleanExpression = 
        And of BooleanExpression * BooleanExpression
        | Or of BooleanExpression * BooleanExpression
        | Not of BooleanExpression
        | Comparison of ComputationExpression * ComputationExpression * ComparisonOperator
        | ValueOfColumn of string
    and ColumnComputationExpression = 
        | Moving of Reduction * windowSize : int * ComputationExpression
        | Expanding of Reduction * ComputationExpression
    and Regression = 
        Linear
    and ComputationExpression = 
        Binary of ComputationExpression * ComputationExpression * BinaryOperator
        | Boolean of BooleanExpression 
        | IfThisThenElse of BooleanExpression * ComputationExpression * ComputationExpression
        | FormatDate of columnName:string * DateFormat
        | Number of Number
        | Int of ComputationExpression
        | RegularExpression of input: ComputationExpression * pattern : string * resultFormation: RegExResultToken list
        | Keys
        | MissingValue
        | String of string
        | DateTime of System.DateTime
        | ColumnName of string
        | ColumnExpression of ColumnComputationExpression
        | Regression of Regression * ComputationExpression * ComputationExpression
        | Extrapolate of Regression * ComputationExpression * int * trainingLength: int option
        | Ordinals
    
    type FilterAndSorting =
       SliceColumns of string list 
       | DenseColumns
       | DenseRows  
       | NumericColumns
       | IndexBy of ComputationExpression
       | SortBy of columnName : string
       | Only of condition: BooleanExpression

    type ColumnStatements = 
        CreateColumn of ComputationExpression * string
        | RenameColumn of string * string
        | Pivot of rowkey: ComputationExpression * columnkey: ComputationExpression * value : ComputationExpression * Reduction

    type Statement = 
        Reduction of Reduction
        | FilterAndSorting of FilterAndSorting
        | Cluster of Cluster
        | Column of ColumnStatements

    type Value = 
        | Mapping of Map<Value, Value> 
        | Sequence of Value list
        | String of string
        | Boolean of bool
        | Decimal of decimal
        | Null

    type Block = 
        Statements of Statement list
        | Comment of string
        | Source of source:string * properties:Map<string,Value>

    [<CustomEquality>]
    [<CustomComparison>]
    type KeyType =
        Numbers of float
        | Text of string
        | DateTime of System.DateTime
        | Obj of obj
        | List of KeyType list
        | Missing
        with 
            static member UnWrap = 
                function 
                    Numbers n -> n |> box
                    | Text s -> s :> obj
                    | DateTime d -> d :> obj
                    | Obj o -> 
                        match o with
                        :? KeyType as k ->
                             k |> KeyType.UnWrap
                        | :? string as s -> s :> obj
                        | :? System.DateTime as d -> d :> obj
                        | :? System.UInt16 as a -> 
                            a |> decimal |> box
                        | :? System.UInt32 as a -> 
                            a |> decimal |> box
                        | :? System.UInt64 as a -> 
                            a |> decimal |> box
                        | :? System.Int16 as a -> 
                            a |> decimal |> box
                        | :? System.Int32 as a -> 
                            a |> decimal |> box
                        | :? System.Int64 as a -> 
                            a |> decimal |> box
                        | :? System.Single as a -> 
                            a |> decimal |> box
                        | :? System.Double as a -> 
                            a |> decimal |> box
                        | :? System.Decimal as a -> 
                            a |> box
                        | _ -> o
                    | List lst -> 
                        lst |> List.map KeyType.UnWrap :> obj
                    | Missing -> null     
            static member OfOption (o : 'a option) = 
                match o with
                None -> Missing
                | Some v -> 
                    v |> KeyType.Create
            static member Create (i : obj) = 
                match i with
                null -> Missing
                | :? KeyType as k -> k
                | :? string as s -> Text(s)                
                | :? System.DateTime as d ->
                    DateTime(d)                
                | :? System.UInt16 as a -> 
                    Numbers(a |> float)
                | :? System.UInt32 as a -> 
                    Numbers(a |> float)
                | :? System.UInt64 as a -> 
                    Numbers(a |> float)
                | :? System.Int16 as a -> 
                    Numbers(a |> float)
                | :? System.Int32 as a -> 
                    Numbers(a |> float)
                | :? System.Int64 as a -> 
                    Numbers(a |> float)
                | :? System.Single as a -> 
                    Numbers(a |> float)
                | :? System.Double as a -> 
                    Numbers(a)
                | :? System.Decimal as a -> 
                    Numbers(a |> float)
                | :? (KeyType list) as l ->
                    List(l)
                | :? System.Collections.IEnumerable as l ->
                    [for item in l -> KeyType.Create item]
                    |> List
                | o -> Obj(o)
            interface System.IComparable with
                member x.CompareTo(other) = 
                    match other with
                    :? KeyType as otherKey ->
                        let result = 
                            match x,otherKey with
                            Numbers n1,Numbers n2 -> n1.CompareTo n2
                            | Text t1, Text t2 -> t1.CompareTo t2
                            | List lst1, List lst2 -> 
                                lst1
                                |> List.zip lst2
                                |> List.tryFind(fun (a,b) -> a <> b)
                                |> Option.bind(fun (a,b) -> (a :> System.IComparable).CompareTo b |> Some)
                                |> Option.orElse(Some 0)
                                |> Option.get
                            | DateTime dt1,DateTime dt2 -> dt1.CompareTo dt2
                            | Missing,Missing -> 0
                            | Missing,_ -> -1
                            | _,Missing -> 1
                            | a,b -> failwithf "Can't compare %A to %A" a b
                        result
                       
                    | _ -> -1
            override x.Equals(o) = 
                (x :> System.IComparable).CompareTo(o) = 0
            override x.GetHashCode() =
                match x with
                Numbers d -> d.GetHashCode()
                | Text d -> d.GetHashCode()
                | DateTime d -> d.GetHashCode()
                | Obj d -> d.GetHashCode()
                | List d -> d.GetHashCode()
                | Missing -> 0
                