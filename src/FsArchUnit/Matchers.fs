namespace FsArchUnit

open System
open System.Text.RegularExpressions
open Mono.Cecil

[<AutoOpen>]
module Matchers =
    let Simple f l = Matcher<TypeDefinition>.SimpleMatcher {Value = f; Label = l; Result = None}
    
    let rec GetNamespace(t: TypeDefinition) :string =
        match (t.IsNested, t.Namespace |> Option.ofObj, t.DeclaringType |> Option.ofObj) with
            | true, None, Some dt -> GetNamespace dt
            | _ -> t.Namespace

    let ResideInNamespace (ns: string)=        
        Simple (fun t -> (GetNamespace t).StartsWith(ns)) $"ResideInNamespace {ns}"
        
    let HaveNameStartingWith(s: string) =
        Simple (fun t -> t.Name.StartsWith(s) ) $"HaveNameStartingWith {s}"
                            
    let HaveNameEndingWith(s: string) =
        Simple (fun t -> t.Name.EndsWith(s) ) $"HaveNameEndingWith {s}"

    let HaveName(s: string) =
        Simple (fun t -> t.Name.Equals(s, StringComparison.InvariantCultureIgnoreCase) ) $"HaveName {s}"
                    
    let HaveNameMatching(pattern: string) =
        Simple (fun t -> Regex(pattern).Match(t.Name).Success ) $"HaveNameMatching {pattern}"
    
    
    type TypeDefinitionProperty =
        | Interface
        | Class
        | Sealed
        | Public
        | Nested
        | Generic
        | Abstract
        with member x.Get(t: TypeDefinition ): bool =
                match x with
                    | Interface -> t.IsInterface
                    | Class -> t.IsClass
                    | Sealed -> t.IsSealed
                    | Public -> t.IsPublic
                    | Nested -> t.IsNested
                    | Generic -> t.HasGenericParameters
                    | Abstract -> t.IsAbstract
        
    let Be (property: TypeDefinitionProperty) =
        Simple property.Get $"Be {property.ToString()}"
        
    let Are = Be

    let private ToTypeDefinition(t: Type) =
        let assembly = System.Reflection.Assembly.GetAssembly(t);
        let assemblyDef = Uri.UnescapeDataString(assembly.Location)
                            |> AssemblyDefinition.ReadAssembly
         

        assemblyDef.Modules
                |> Seq.collect (fun m -> m.Types)
                |> Seq.find (fun td -> td.IsClass && td.Namespace <> null && td.FullName.Equals(t.FullName))
        
            
    let rec private BaseClasses(t: TypeDefinition) =
        match Option.ofObj t.BaseType with
            | None -> Seq.empty
            | Some tr ->
                let baseType = tr.Resolve()
                seq {
                    yield baseType
                    yield! (baseType |> BaseClasses )
                }
    
    let private IsSubclassOf (child: TypeDefinition) (parent: TypeDefinition) =
        if (parent <> null) then
            child.MetadataToken <> parent.MetadataToken &&
                child |> BaseClasses |> Seq.exists (fun b -> b.MetadataToken = parent.MetadataToken)
        else
            false
                     
    let Inherit(baseClass: Type) =
        let baseClassDef = baseClass |> ToTypeDefinition
        Simple (fun t -> (IsSubclassOf t baseClassDef) ) $"Inherit {baseClass.FullName}"

    let ImplementInterface (interfaceType: Type) =
        if (not interfaceType.IsInterface) then
            raise (ArgumentException $"The type {interfaceType.FullName} is not an interface.")
        else
            Simple (fun t -> ( t.Interfaces |> Seq.exists (fun i -> interfaceType.FullName = i.InterfaceType.FullName)) )
                $"ImplementInterface {interfaceType.FullName}"

    let HaveCustomAttribute(attribute: Type) =
        Simple (fun t -> (t.CustomAttributes |> Seq.exists (fun a -> attribute.FullName = a.AttributeType.FullName)))
                $"HaveCustomAttribute {attribute.FullName}"


//    let HaveDependencyOn (m: string -> bool): Matcher =
//
//        fun t -> match CheckDependency t with
//                    | true -> Pass
//                    | false -> FailedReasons [$"Should has dependency of {attribute.FullName}."]
//
//        return new Filter(input => 
//            new DependencySearch(match)
//                .FindTypesWithDependenciesMatch(input.ToList())
//                .GetResults());
//
//    let HaveDependencyOn(g: Globbing ) =
//        HaveDependencyOn(g.matchFun);

    


