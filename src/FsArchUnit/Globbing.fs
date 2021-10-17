namespace FsArchUnit

open System.Text.RegularExpressions

type Globbing (m: string -> bool, label: string) =        
    member internal x.Label = label
    member internal x.MatchFunc = m
    static member internal New (pattern: string) = 
        Globbing(fun name ->
            let regexPattern = $"^{Regex.Escape(pattern)}$".Replace(@"\*", ".*").Replace(@"\?",".");
            if Regex(regexPattern).Match(name).Success then
                true
            else
                if (pattern.EndsWith(".*")) then
                    let newPattern = $"^{Regex.Escape(pattern.Substring(0, pattern.Length - 2))}$".Replace(@"\*", ".*").Replace(@"\?", ".");
                    Regex(newPattern).Match(name).Success
                else
                    false
        , pattern)
       
    static member (+) (lhs: Globbing, rhs: Globbing) =
        Globbing((fun t -> lhs.MatchFunc(t) || rhs.MatchFunc(t)), $"{lhs.Label} + {rhs.Label}")
        
    static member (-) (lhs: Globbing, rhs: Globbing) =
        Globbing ((fun t ->lhs.MatchFunc t && ( (rhs.MatchFunc >> not) t)) , $"{lhs.Label} - {rhs.Label}")
    
module Globbing =
    //    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
    let g = Globbing.New
