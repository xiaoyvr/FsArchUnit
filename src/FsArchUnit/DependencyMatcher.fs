namespace FsArchUnit

open Mono.Cecil

module Matchers =        
    let private memorize fn =
        let cache = System.Collections.Generic.Dictionary<_,_>()
        (fun x ->
            match cache.TryGetValue x with
            | true, v -> v
            | false, _ ->
                let v = fn x
                cache.Add(x, v)
                v)
    
    let HaveDependencyOn (g: Globbing) =
        
        let matchFunc fullName = match g.MatchFunc fullName with
                                    | true -> Some(fullName)
                                    | false -> None
                                    
        let inline matchFullName t =
            let fullName = (^t : (member FullName: string ) t)
            matchFunc fullName
        
        let CheckGenericParameters (parameters: GenericParameter seq) =
            parameters |> Seq.map matchFullName
            
        let CheckTypeReference (r:TypeReference) =
            if r.IsGenericParameter then
                Seq.empty
            else
                match r with
                    | :? GenericInstanceType as g ->
                        g.GenericArguments
                            |> Seq.map matchFullName
                            |> Seq.append (CheckGenericParameters r.GenericParameters)
                    |_ -> Seq.singleton (matchFullName r)
                    
        let CheckTypeReference = memorize CheckTypeReference                

        let CheckCustomAttributes (attrs: CustomAttribute seq) =
            seq {
                for attr in attrs do
                    yield! CheckTypeReference(attr.AttributeType)                    
                    let args = attr.Fields
                               |> Seq.append attr.Properties
                               |> Seq.map (fun na -> na.Argument)
                               |> Seq.append attr.ConstructorArguments
                    for arg in args do
                        match arg.Value with
                            | :? TypeDefinition as t -> yield! CheckTypeReference t
                            | _ -> do ()
            }
            
        let CheckProperties (t:TypeDefinition) =
            seq {
                if t.HasProperties then
                    for property in t.Properties do
                        if property.ContainsGenericParameter then
                            yield! CheckGenericParameters(property.PropertyType.GenericParameters);
                        yield matchFullName property.PropertyType
            }

        let CheckFields (t:TypeDefinition) =
            seq {
                if t.HasFields then
                    for  field in t.Fields do
                        if field.ContainsGenericParameter then
                            yield! CheckGenericParameters field.FieldType.GenericParameters
                        yield matchFullName field.FieldType 
            }
            
        let CheckMethodParameters (parameters:ParameterDefinition seq) =
            seq {
                for parameter in parameters do
                    if parameter.ParameterType <> null then
                        yield! CheckTypeReference(parameter.ParameterType)
            }
                
        let CheckMethodBody (method: MethodDefinition) = 
            seq {
                if method.HasBody then
                    for variable in method.Body.Variables do
                        // Check any nested types in methods - the compiler will create one for every asynchronous method or iterator. 
                        if variable.VariableType.IsNested then
                            yield! CheckTypeReference variable.VariableType
                        else
                            if variable.VariableType.ContainsGenericParameter then
                                yield! CheckGenericParameters variable.VariableType.GenericParameters
                            
                            yield matchFullName variable.VariableType

                    // Check each instruction for references to our types
                    for instruction in method.Body.Instructions do
                        if (instruction.Operand <> null) then
                            match instruction.Operand with
                                | :? MemberReference as mf -> 
                                    if (mf.DeclaringType <> null) then
                                        yield! CheckTypeReference mf.DeclaringType
                                | _ -> do ()
            }
        
        let CheckMethod  (method: MethodDefinition) =
            seq {
                yield! CheckCustomAttributes method.CustomAttributes
                if method.ReturnType.ContainsGenericParameter then
                    yield! CheckGenericParameters method.ReturnType.GenericParameters

                yield matchFullName method.ReturnType
                
                yield! CheckMethodParameters method.Parameters

                if method.ContainsGenericParameter then
                    yield! CheckGenericParameters(method.GenericParameters)
                
                yield! CheckMethodBody(method);
            }
        
        let CheckEvents (t:TypeDefinition) =
            seq {
                if t.HasEvents then
                    for eventDef in t.Events do
                        if eventDef.HasOtherMethods then
                            for method in eventDef.OtherMethods do               
                                yield! CheckMethod(method)    
            }
        
        let CheckTypeDefinition (t:TypeDefinition) =
            seq {
                yield! CheckCustomAttributes t.CustomAttributes
                if (t.BaseType <> null) then
                    yield! CheckTypeReference t.BaseType
                yield! CheckProperties t
                
                if t.HasGenericParameters then
                    yield! CheckGenericParameters t.GenericParameters

                yield! CheckFields t
                yield! CheckEvents t

        //        for nested in t.NestedTypes do
        //            CheckTypeDefinition nested
                
                for method in t.Methods do
                    yield! CheckMethod method                
            }
            
        let CheckTypeDefinition = memorize CheckTypeDefinition
        
        let CheckDependency t =            
            CheckTypeDefinition t |> Seq.choose id |> (Seq.isEmpty >> not)
        
        Matchers.Simple CheckDependency $"HaveDependencyOn {g.Label}"
