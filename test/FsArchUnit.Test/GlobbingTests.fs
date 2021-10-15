module FsArchUnit.Test.GlobbingTests

open FsArchUnit
open Xunit
open FsArchUnit.Clause
open FsUnit.Xunit
open FsArchUnit.Matchers

let trueMatcher = Simple (fun _ -> true) "true"
let falseMatcher = Simple (fun _ -> false) "false"

[<Fact>]
let ``should return all if all matcher are fail`` () =
    let violations = Types.InCurrentDomain()
                     |> That trueMatcher
                     |> Should (Be Sealed)
                     |> Check
                     
    violations |> should not' Empty
