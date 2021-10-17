module FsArchUnit.Test.DependencyTests

open FsArchUnit
open Xunit
open FsUnit.Xunit

let trueMatcher = Simple (fun _ -> true) "true"
let falseMatcher = Simple (fun _ -> false) "false"

[<Fact>]
let ``should check positives`` () =    
    let violations = Types.InNamespace("FsArchUnit.Test.Fixtures.HaveDependencyOn.Positives")
                     |> That trueMatcher
                     |> Should (HaveDependencyOn (g "FsArchUnit.Test.Fixtures.HaveDependencyOn.Dependencies.*"))
                     |> Check                 
    violations |> should be Empty
    
[<Fact>]
let ``should check negatives`` () =    
    let violations = Types.InNamespace("FsArchUnit.Test.Fixtures.HaveDependencyOn.Negatives")
                     |> That trueMatcher
                     |> Should (Not (HaveDependencyOn (g "FsArchUnit.Test.Fixtures.HaveDependencyOn.Dependencies.*")))
                     |> Check                 
    violations |> should be Empty
        
[<Fact>]
let ``should check self`` () =
    let violations = Types.InNamespace("FsArchUnit.Test.Fixtures.HaveDependencyOn.Dependencies")
                     |> That (HaveName "BaseClass")
                     |> Should (HaveDependencyOn (g "FsArchUnit.Test.Fixtures.HaveDependencyOn.Dependencies.*"))
                     |> Check                 
    violations |> should be Empty
