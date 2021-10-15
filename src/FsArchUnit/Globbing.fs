namespace FsArchUnit

open System.Text.RegularExpressions

type Globbing (m: string -> bool) =        
    member x.matchFun = m
    static member New (pattern: string) = 
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
        )
       
    static member (+) (lhs: Globbing, rhs: Globbing) =        
        Globbing(fun t -> lhs.matchFun(t) || rhs.matchFun(t))
        
    static member (-) (lhs: Globbing, rhs: Globbing) =
        Globbing(fun t ->
            lhs.matchFun t && ( (rhs.matchFun >> not) t))
    
    
    
module Globbing =
    //    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
    let g = Globbing.New
