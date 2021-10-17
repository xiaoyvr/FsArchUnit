namespace FsArchUnit.Test.Fixtures.F01

type F01ClassA = class end

type Fx1ClassA = class end


namespace FsArchUnit.Test.Fixtures.Nested

module F02ModuleA =
    type Nested = class end
    

namespace FsArchUnit.Test.Fixtures.Or
type ClassNameEndWithA = class end
type ClassNameEndWithB = class end


namespace FsArchUnit.Test.Fixtures.And
type ClassA = class end

namespace FsArchUnit.Test.Fixtures.Not
type ClassNameEndWithA = class end
type ClassNameEndWithB = class end

namespace FsArchUnit.Test.Fixtures.Inherit
type BaseClass() =
    member x.Some = "123"

namespace FsArchUnit.Test.Fixtures.Inherit.SubClasses
type SuccessCase() =
    inherit FsArchUnit.Test.Fixtures.Inherit.BaseClass()
type FailCase() = class end
    
namespace FsArchUnit.Test.Fixtures.ImplementInterface
type ISomeInterface =interface end
type SuccessCase() =
    interface ISomeInterface    
type FailCase() = class end

// HaveCustomAttribute
namespace FsArchUnit.Test.Fixtures.HaveCustomAttribute
type SomeAttribute() =
    inherit System.Attribute()

[<Some>]    
type SuccessCase() = class end
type FailCase() = class end



// HaveDependencyOn
namespace FsArchUnit.Test.Fixtures.HaveDependencyOn
open System
type IGenericInterface<'a> = interface end
type GenericBaseClass<'a> () = class end
type OneParamAttribute(_someType :Type) = inherit Attribute()

namespace FsArchUnit.Test.Fixtures.HaveDependencyOn.Dependencies

type BaseClass() = class end
type GenericBaseClass<'a>() = class end
type ISomeInterface = interface end



type ISomeGenericInterface<'a> = interface end
type Model = class end

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



namespace FsArchUnit.Test.Fixtures.HaveDependencyOn.Negatives
open FsArchUnit.Test.Fixtures.HaveDependencyOn
type Case() = class end
type CaseGenericInterface() =
    interface IGenericInterface<Case>

