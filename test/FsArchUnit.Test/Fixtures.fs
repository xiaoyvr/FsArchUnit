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

namespace FsArchUnit.Test.Fixtures.HaveCustomAttribute
type SomeAttribute() =
    inherit System.Attribute()

[<Some>]    
type SuccessCase() = class end
type FailCase() = class end
