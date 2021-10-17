module FsArchUnit.Test.MatchersTests


open FsArchUnit
open Mono.Cecil
open Xunit
open FsUnit.Xunit

let HaveNameTestData () =
    seq {
        yield [|box "NotExist"; box False|]
        yield [|"ClassA"; True|]
    }

let ofSimple matcher =
    match matcher with
        | SimpleMatcher m -> m
        | _ -> failwith "Not a simple matcher" 

[<Theory>]
[<MemberData(nameof(HaveNameTestData))>]
let ``should check HaveName`` testName target =
    let matcher = HaveName testName |> ofSimple
    matcher.Value (TypeDefinition( "NS", "ClassA" ,TypeAttributes.Class)) |> should be target

let HaveNameMatchingTestData () =
    seq {
        yield [|box ".*A"; box True|]
        yield [|".*B"; False|]
    }    
[<Theory>]
[<MemberData(nameof(HaveNameMatchingTestData))>]    
let ``should check HaveNameMatching`` testName target =
    let matcher = HaveNameMatching testName |> ofSimple
    matcher.Value (TypeDefinition( "NS", "ClassA" ,TypeAttributes.Class)) |> should be target
    
[<Fact>]    
let ``should check Be`` () =
    let matcher = Be Public |> ofSimple
    matcher.Value (TypeDefinition( "NS", "ClassA" ,(TypeAttributes.Class ||| TypeAttributes.Public))) |> should be True
    matcher.Value (TypeDefinition( "NS", "ClassA" ,(TypeAttributes.Class ||| TypeAttributes.NotPublic))) |> should be False
    
[<Fact>]
let ``should check Inherit`` () =    
    let violations = Types.InNamespace("FsArchUnit.Test.Fixtures.Inherit.SubClasses")
                        |> Should (Inherit typedefof<FsArchUnit.Test.Fixtures.Inherit.BaseClass>)  
                        |> Check
                        
    violations |> should haveLength 1
    violations.[0].TypeName |> should haveSubstring "FsArchUnit.Test.Fixtures.Inherit.SubClasses.FailCase"
    violations.[0].Reason |> should haveSubstring "Inherit FsArchUnit.Test.Fixtures.Inherit.BaseClass"
                        

[<Fact>]
let ``should check ImplementInterface`` () =    
    let violations = Types.InNamespace("FsArchUnit.Test.Fixtures.ImplementInterface")
                        |> That (Are Class)
                        |> Should (ImplementInterface typedefof<FsArchUnit.Test.Fixtures.ImplementInterface.ISomeInterface>)  
                        |> Check
                        
    violations |> should haveLength 1
    violations.[0].TypeName |> should haveSubstring "FsArchUnit.Test.Fixtures.ImplementInterface.FailCase"
    violations.[0].Reason |> should haveSubstring "ImplementInterface FsArchUnit.Test.Fixtures.ImplementInterface.ISomeInterface"
                        
[<Fact>]
let ``should check HaveCustomAttribute`` () =    
    let violations = Types.InNamespace("FsArchUnit.Test.Fixtures.HaveCustomAttribute")
                        |> That ((Are Class) |> And <| (Not (HaveNameEndingWith "Attribute") ))
                        |> Should (HaveCustomAttribute
                                       typedefof<FsArchUnit.Test.Fixtures.HaveCustomAttribute.SomeAttribute>)  
                        |> Check
                        
    violations |> should haveLength 1
    violations.[0].TypeName |> should haveSubstring "FsArchUnit.Test.Fixtures.HaveCustomAttribute.FailCase"
    violations.[0].Reason |> should haveSubstring "HaveCustomAttribute FsArchUnit.Test.Fixtures.HaveCustomAttribute.SomeAttribute"
                        

    