namespace FsArchUnit
open Mono.Cecil
                    
type Violation = {
    TypeName: string
    Reason: string
}

type IncompleteTarget = {
    Types: TypeDefinition seq
    Filter: Matcher<TypeDefinition> option
    Matcher: Matcher<TypeDefinition> option
} with static member Create types =
        {Types = types; Filter = None; Matcher = None}

type Target = {
    Types: TypeDefinition seq
    Filter: Matcher<TypeDefinition> option
    Matcher: Matcher<TypeDefinition>
}


module Clause =
//    let inline private uplift (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
    
    let That filter ts : IncompleteTarget =
        {ts with Filter = Some(filter) }

    let Should matcher t =
        let {IncompleteTarget.Types = types; IncompleteTarget.Filter = filter}  = t        
        {Types = types; Filter = filter; Matcher = matcher} 

    let And lhs rhs =
        Matcher.AndMatcher{ Lhs = lhs; Rhs = rhs ;Result = None } 
    let Or lhs rhs =
        Matcher.OrMatcher{ Lhs = lhs; Rhs = rhs ;Result = None }

    let Not m = Matcher.NotMatcher{ Child = m ;Result = None }
    
    let Check (target:Target)  =
        target.Types
        |> Seq.filter (fun t ->
            match target.Filter with
                | None -> true
                | Some f -> (Matcher.Exec f t).Result |> Option.defaultValue true )
        |> Seq.map(
            fun t ->
                let matcherWithResult = Matcher.Exec target.Matcher t
                let result = matcherWithResult.Result |> Option.defaultValue true
                match result with
                    | true -> None
                    | false -> Some {TypeName = t.FullName
                                     Reason = (Matcher.Trace matcherWithResult) |> Option.defaultValue "!!!None!!!"  })
        |> Seq.choose id 
        |> List.ofSeq
