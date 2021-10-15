module FsArchUnit.Spike

open Mono.Cecil
open FsArchUnit.Clause

module Bla =
            
    let ResideInNamespace str (td: TypeDefinition) =
        true
    let HaveDependencyOn str td =
        true
        
    let rxfull = fun td ->
        (   (ResideInNamespace "NetArchTest.SampleLibrary.Presentation")
            >> (
                    (
                        (ResideInNamespace "NetArchTest.SampleLibrary.Presentation") >> (||)
                    ) td
                )
        ) td
        
    
    let violations = Types.InCurrentDomain()
                    |> that
                           ( not << (ResideInNamespace "NetArchTest.SampleLibrary.Presentation")
                                |> or' <| ResideInNamespace "NetArchTest.SampleLibrary.Presentation"
                                |> or' <| ResideInNamespace "NetArchTest.SampleLibrary.Presentation"  )
                        
                    |> should (HaveDependencyOn("NetArchTest.SampleLibrary.Data"))
                    |> check
        
    
    
    