namespace FsArchUnit.Test.Fixtures.HaveDependencyOn
open System
type IGenericInterface<'a> = interface end
type GenericBaseClass<'a> () = class end
type OneParamAttribute(_someType :Type) =
    inherit Attribute()
type OnePropertyAttribute() =
    inherit Attribute()
    member val Property = Unchecked.defaultof<Type> with get, set
type OneFieldAttribute() =
    inherit Attribute()
    [<DefaultValue>]
    val mutable Field: Type


namespace FsArchUnit.Test.Fixtures.HaveDependencyOn.Dependencies

type BaseClass() = class end
type GenericBaseClass<'a>() = class end
type ISomeInterface = interface end
type ISomeGenericInterface<'a> = interface end
type Model() = class end
type SomeAttribute() = inherit System.Attribute()


namespace FsArchUnit.Test.Fixtures.HaveDependencyOn.Positives
open FsArchUnit.Test.Fixtures.HaveDependencyOn
type CaseInherit() = inherit Dependencies.BaseClass()
type CaseGenericInherit() = inherit Dependencies.GenericBaseClass<string>()
type CaseInheritGenericParameter() =
    inherit GenericBaseClass<Dependencies.Model>()
type CaseInterface() = interface Dependencies.ISomeInterface
type CaseGenericInterface() = interface Dependencies.ISomeGenericInterface<string>
type CaseInterfaceGenericParameter() =
    interface IGenericInterface<Dependencies.Model>
[<Dependencies.Some>]
type CaseAttribute() = class end
[<OneParam(typedefof<Dependencies.Model>)>]
type CaseAttributeParameter() = class end
[<OneProperty(Property = typedefof<Dependencies.Model>)>]
type CaseAttributeProperty() = class end
[<OneField(Field = typedefof<Dependencies.Model>)>]
type CaseAttributeField() = class end
type CaseProperty() =
    member val Property = Dependencies.Model() with get, set
type CaseGenericProperty() =
    member val Property: IGenericInterface<Dependencies.Model> =
        Unchecked.defaultof<IGenericInterface<Dependencies.Model>> with get, set
type CaseField() =
    [<DefaultValue>]
    val mutable Field: Dependencies.Model
type CaseGenericField() =
    [<DefaultValue>]
    val mutable Field: IGenericInterface<Dependencies.Model>
    
// method
type CaseMethodReturnType() =
    let PrivateMethod():Dependencies.Model =
        Dependencies.Model()
type CaseMethodGenericReturnType() =
    static let StaticPrivateMethod():IGenericInterface<Dependencies.Model> =
        Unchecked.defaultof<IGenericInterface<Dependencies.Model>>
        
type CaseMethodParameter() =
    member x.PublicMethod(m: Dependencies.Model)=
        ()
type CaseMethodGenericParameter() =        
    member x.PublicMethod(m: IGenericInterface<Dependencies.Model>)=
        ()
        
type CaseMethodBody() =
    static member PublicMethod() =
        let m = Dependencies.Model()
        ()
        
type CaseMethodGenericBody() =
    static member PublicMethod() =
        let m = GenericBaseClass<Dependencies.Model>()
        ()

module CaseNestedType =
   let c = Dependencies.BaseClass()
   type NestedType() =
       inherit Dependencies.BaseClass()
       
module CaseNestedNestedType =
   let c = Dependencies.BaseClass()
   module NestedType =
       let c = Dependencies.BaseClass()
       type NestedNestedType() = 
           inherit Dependencies.BaseClass()
           
type CaseNestedGeneric() =
    inherit GenericBaseClass<GenericBaseClass<Dependencies.Model>>()
    
// events; not really used much, can come later


namespace FsArchUnit.Test.Fixtures.HaveDependencyOn.Negatives
open FsArchUnit.Test.Fixtures.HaveDependencyOn
type Case() = class end
type CaseGenericInterface() = interface IGenericInterface<Case>
