module Tests

open FsArchUnit
open FsArchUnit.Clause
open FsArchUnit.Matchers
open Xunit
open FsUnit.Xunit

let trueMatcher = Simple (fun _ -> true) "true"
let falseMatcher = Simple (fun _ -> false) "false"

[<Fact>]
let ``should return all if all matcher are fail`` () = 
    let violations = Types.InCurrentDomain()
                     |> That trueMatcher
                     |> Should falseMatcher
                     |> Check

    violations |> should not' Empty
                    
[<Fact>]
let ``should return no types if all matcher are success`` () =
    let violations = Types.InCurrentDomain()
                     |> That trueMatcher
                     |> Should trueMatcher
                     |> Check
                     
    violations |> should be Empty
                    
[<Fact>]
let ``should have reason for violations`` () =
    
    let testMatcher = Simple (fun t -> t.Name.StartsWith("F01") ) "namestartwith \"F01\""
    
    let violations = Types.InNamespace("FsArchUnit.Test.Fixtures.F01")
                     |> That trueMatcher
                     |> Should testMatcher
                     |> Check

    violations |> should haveLength 1
    let violation = violations.[0]
    violation.TypeName |> should equal "FsArchUnit.Test.Fixtures.F01.Fx1ClassA"
    violation.Reason |> should equal "namestartwith \"F01\""
    
[<Fact>]
let ``should not need the filter part`` () =
    let violations = Types.InCurrentDomain()
                     |> Should falseMatcher
                     |> Check
                     
    violations |> should not' Empty
    
[<Fact>]
let ``should apply filter if there's filter part`` () =
    let testMatcher1 = Simple (fun t -> t.Name <> "Fx1ClassA" ) "name is not \"Fx1ClassA\""
    let violations = Types.InNamespace("FsArchUnit.Test.Fixtures.F01")
                     |> That testMatcher1
                     |> Should (HaveNameStartingWith "F01")
                     |> Check
                     
    violations |> should be Empty
    
[<Fact>]
let ``should be able to find the nested types`` () =
    
    let violations = Types.InNamespace("FsArchUnit.Test.Fixtures.F02")
                     |> That trueMatcher
                     |> Should (HaveNameStartingWith "F02")
                     |> Check
    let violation = violations |> List.tryFind (fun v -> v.TypeName = "FsArchUnit.Test.Fixtures.F02.F02ModuleA/Nested")
    violation |> should not' None
    
