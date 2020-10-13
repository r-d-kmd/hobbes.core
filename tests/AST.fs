namespace Hobbes.Tests

open Xunit

module AST =
    
    [<Fact>]
    let KeyComparison() =
        let keys = 
            [
                1 :> obj
                "two" :> obj
                [3;4] :> obj
            ] |> List.map(fun k -> Hobbes.Parsing.AST.KeyType.Create k :> System.IComparable)
        keys
        |> List.iter(fun key -> Assert.Equal(key,key))