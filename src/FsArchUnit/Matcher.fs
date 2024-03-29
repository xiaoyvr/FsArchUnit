﻿namespace FsArchUnit

type Terminal<'a, 'r> = {Value: 'a ; Label: string; Result: 'r option}
type Binary<'b, 'r> = { Lhs : 'b; Rhs: 'b; Result: 'r option}
type Unary<'b, 'r> = { Child : 'b; Result: 'r option}

//type Tree<'a, 'r> =
//    | Leaf of Terminal<'a, 'r>
//    | BinaryB of Binary<Tree<'a, 'r>, 'r >
//    | UnaryB of Unary<Tree<'a, 'r>, 'r >


type Matcher<'a> =
    | SimpleMatcher of Terminal<'a -> bool, bool>
    | NotMatcher of Unary<Matcher<'a>, bool>
    | AndMatcher of Binary<Matcher<'a>, bool>
    | OrMatcher of Binary<Matcher<'a>, bool>
    with member internal x.Result =
            match x with
            | SimpleMatcher {Result = r}
            | NotMatcher {Result = r}
            | AndMatcher {Result = r}
            | OrMatcher {Result = r} -> r

module internal Matcher = 
    let rec Exec matcher input =
        match matcher with
            | SimpleMatcher {Value = f; Label = l } ->
                SimpleMatcher {Value = f; Label = l; Result = Some(f input)} 
            | AndMatcher {Lhs = lhs; Rhs = rhs } ->
                let l = (Exec lhs input)
                let r = (Exec rhs input)
                let result = match l.Result, r.Result with
                                | Some(lr), Some(rr) -> Some(lr && rr)
                                | Some(false), None | None, Some(false) -> Some(false)
                                | _ -> None 
                AndMatcher {Lhs = l; Rhs = r ; Result = result }
            | OrMatcher {Lhs = lhs; Rhs = rhs } ->
                let l = (Exec lhs input)
                let r = (Exec rhs input)
                let result = match l.Result, r.Result with
                                | Some(lr), Some(rr) -> Some(lr || rr)
                                | Some(true), None | None, Some(true) -> Some(true)
                                | _ -> None 
                OrMatcher {Lhs = l; Rhs = r ; Result = result }
            | NotMatcher {Child = c} ->
                let r = (Exec c input)
                NotMatcher {Child = r; Result = Option.map not r.Result}

    let rec Trace matcher =
        match matcher with
            | SimpleMatcher {Label = l} ->
                Some l
            | AndMatcher {Lhs = lhs; Rhs = rhs } ->
                match lhs.Result, rhs.Result with
                    | Some true, Some true -> Some $"({(Trace lhs).Value} and {(Trace rhs).Value})"
                    | _, Some false -> (Trace rhs)
                    | Some false, _ -> (Trace lhs)
                    | _ -> None                                    
            | OrMatcher {Lhs = lhs; Rhs = rhs } ->
                match lhs.Result, rhs.Result with
                    | Some false, Some false -> Some $"({(Trace lhs).Value} or {(Trace rhs).Value})"
                    | Some true, _ -> (Trace lhs)
                    | _, Some true -> (Trace rhs)
                    | _ -> None
            | NotMatcher {Child = c } ->
                Some $"not {(Trace c).Value}" 
