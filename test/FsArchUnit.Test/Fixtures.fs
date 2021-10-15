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
