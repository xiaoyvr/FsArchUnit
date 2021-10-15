namespace FsArchUnit

open System
open System.Text.RegularExpressions
open Mono.Cecil

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
//
//    let HaveName(s: string): Terminal =
//        fun t -> match t.Name.Equals(s, StringComparison.InvariantCultureIgnoreCase) with
//                    | true -> Pass
//                    | false -> FailedReasons [$"Name should be {s}"]
//                    
//    let HaveNameMatching(pattern: string): Terminal =
//        fun t -> match Regex(pattern).Match(t.Name).Success with
//                    | true -> Pass
//                    | false -> FailedReasons [$"Name should match {pattern}"]
    
    
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

    let private ToTypeDefinition(t: Type) =
        let assembly = System.Reflection.Assembly.GetAssembly(t);
        let assemblyDef = Uri.UnescapeDataString(UriBuilder(assembly.CodeBase).Path)
                            |> AssemblyDefinition.ReadAssembly
         

        assemblyDef.Modules
            |> Seq.collect (fun m -> m.Types)
            |> Seq.find (fun t -> t.IsClass && t.Namespace <> null && t.FullName.Equals(t.FullName))
            
    let rec private BaseClasses(t: TypeDefinition) =
        match Option.ofObj t.BaseType with
            | None -> Seq.empty
            | Some tr ->
                let baseType = tr.Resolve()
                seq {
                    yield baseType
                    yield! (baseType |> BaseClasses )
                }
    
    let IsSubclassOf (child: TypeDefinition) (parent: TypeDefinition) =
        if (parent <> null) then
            child.MetadataToken <> parent.MetadataToken &&
                child |> BaseClasses |> Seq.exists (fun b -> b.MetadataToken = parent.MetadataToken)
        else
            false
                     
//    let Inherit(baseClass: Type): Terminal = 
//        let baseClassDef = baseClass |> ToTypeDefinition
//        fun t -> match (IsSubclassOf t baseClassDef) with
//                    | true -> Pass
//                    | false -> FailedReasons [$"Should be subclass of {baseClass.ToString()}."]
//
//
//    let HaveCustomAttribute(attribute: Type): Terminal = 
//        fun t -> match (t.CustomAttributes |> Seq.exists (fun a -> attribute.FullName = a.AttributeType.FullName)) with
//                    | true -> Pass
//                    | false -> FailedReasons [$"Should has attribute of {attribute.FullName}."]
//    
//    
//    let ImplementInterface (interfaceType: Type ): Terminal =
//        if (not interfaceType.IsInterface) then
//            raise (ArgumentException $"The type {interfaceType.FullName} is not an interface.")
//        else
//            fun t -> match ( t.Interfaces |> Seq.exists (fun i -> interfaceType.FullName = i.InterfaceType.FullName)) with
//                        | true -> Pass
//                        | false -> FailedReasons [$"Should has attribute of {interfaceType.FullName}."]

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

    


