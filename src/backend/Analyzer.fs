namespace Hobbes.Parsing

module Analyzer = 
    
    type TransformationInfo = 
      {
          Statement : AST.Statement
          InputColumns : string list 
          OutputColumns : string list
      }
    type SourceInfo = 
      {
          SourceId : string
          Properties : Map<string,string>
      } 
    type TransformationNode = 
       {
           Current: TransformationInfo list
           BranchRoots : TransformationNode list
       }
    type ExecutionTreeRoot = 
        {
            Source : SourceInfo
            TransformationRoot : TransformationNode
        }

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TransformationInfo =
        let create stmt =
            let all = ["__all__"]
            let inCols,outCols = 
                match stmt with 
                AST.Reduction _ ->
                    all, all
                | AST.FilterAndSorting fs ->
                    match fs with
                    AST.SliceColumns cols ->
                       all, cols
                    | AST.DenseColumns -> all,all
                    | AST.DenseRows  -> all,all
                    | AST.NumericColumns -> all,all
                    | AST.IndexBy _ -> all,all
                    | AST.SortBy _ -> all,all
                    | AST.Only _ -> all,all
                | AST.Cluster c ->
                    match c with
                    AST.Buckets _ -> all,all
                    | AST.KMeansClustering _ -> all,all
                    | AST.Each _ -> all,all
                    | AST.GroupBy(columnNames,_) -> columnNames,all
                | AST.Column c ->
                    let getColumns = 
                        failwith "Not implemented"
                    match c with
                    AST.CreateColumn(exp,col) -> getColumns exp,col::all
                    | AST.RenameColumn(a,b) -> [a],b::all //remove a
                    | AST.Pivot(rowKey,columnkey,value,_) -> rowKey::columnkey::[value] |> List.collect getColumns, all
            {
                Statement = stmt
                InputColumns = inCols
                OutputColumns = outCols
            }