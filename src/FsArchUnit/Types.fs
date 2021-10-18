namespace FsArchUnit

open System
open System.Runtime.CompilerServices
open Mono.Cecil
open System.Reflection
open System.IO

module Types =
    let inline private fullName (a: ^T) = ( ^T: (member FullName: string) a )
        
    let private excluded (fullName:string) :bool =
        ["System"; "Microsoft"; "Mono.Cecil"; "netstandard"; "NetArchTest.Rules"; "<Module>"; "xunit" ]
            |> Seq.exists( fun l -> fullName.StartsWith(l, StringComparison.InvariantCultureIgnoreCase) )
                                              
    let rec private GetAllTypesWithNested (types: TypeDefinition seq) =
        types
            |> Seq.filter (not << excluded << fullName)  
            |> Seq.collect (fun t ->
                                        let nestedTypes = t.NestedTypes |> Seq.filter (fun nested ->
                                            not (nested.Interfaces |> Seq.exists (fun i ->
                                                i.InterfaceType.FullName = typedefof<IAsyncStateMachine>.FullName)) )
                                        GetAllTypesWithNested(nestedTypes) |> Seq.append [t])
        
    let private InAssembliesWithFilter (assemblies: Assembly seq) (filter: TypeDefinition -> bool) =
        assemblies |> Seq.filter (fun a -> not a.IsDynamic) |> Seq.collect (fun a ->
            
            UriBuilder(a.CodeBase).Path
                        |> Uri.UnescapeDataString
                        |> AssemblyDefinition.ReadAssembly
                        |> (fun a -> a.Modules |> Seq.collect (fun m -> m.Types) ))
        |> Seq.filter filter
        |> GetAllTypesWithNested |> IncompleteTarget.Create
    
    let InAssemblies (assemblies: Assembly seq) =
        InAssembliesWithFilter assemblies (fun _ -> true)
    
    let InCurrentDomain ()  =
        AppDomain.CurrentDomain.GetAssemblies()
            |> Seq.filter (not << excluded << fullName)
            |> InAssemblies

    let InAssembly (assembly: Assembly) =
        [assembly] |> InAssemblies
        
    let InNamespace name = 
        AppDomain.CurrentDomain.GetAssemblies()
            |> InAssembliesWithFilter
            <| (fun t -> t.Namespace <> null && t.Namespace.StartsWith(name, StringComparison.InvariantCultureIgnoreCase))

    let FromFile filename =
        let assemblyDef = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
                          |> fun dir -> match Path.Combine(dir, filename) with
                                        | d when File.Exists(d) -> Some(AssemblyDefinition.ReadAssembly(d))
                                        | _ -> None
        match assemblyDef with
                | Some(a) -> a.Modules |> Seq.collect(fun m -> m.Types)
                | None -> Seq.empty
            |> GetAllTypesWithNested
