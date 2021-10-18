# FsArchUnit

A fsharp API for that can enforce architectural rules in unit tests.

A rewrite from [BenMorris/NetArchTest](https://github.com/BenMorris/NetArchTest) which is inspired by the [ArchUnit](https://www.archunit.org/) library for Java. 

## Rationale

This project allows you create tests that enforce conventions for class design, naming and dependency in .Net code bases. These can be used with any unit test framework and incorporated into a build pipeline. It uses a fluid API that allows you to string together readable rules that can be used in test assertions.

There are plenty of static analysis tools that can evaluate application structure, but they are aimed more at enforcing generic best practice rather than application-specific conventions. The better tools in this space can be press-ganged into creating custom rules for a specific architecture, but the intention here is to incorporate rules into a test suite and create a *self-testing architecture*.

The project is inspired by [ArchUnit](https://www.archunit.org/), a java-based library that attempts to address the difficulties of preserving architectural design patterns in code bases over the long term. Many patterns can only be enforced by convention, which tends to rely on a rigorous and consistent process of code review. This discipline often breaks down as projects grow, use cases become more complex and developers come and go.

## Examples

```f#
// Classes in the presentation should not directly reference repositories
let violations = Types.InCurrentDomain()
                 |> That (ResideInNamespace "SampleLibrary.Presentation")
                 |> Should (Not (HaveDependencyOn "SampleLibrary.Data"))
                 |> Check

// Classes in the "data" namespace should implement IRepository
let violations = Types.InCurrentDomain()
                 |> That ((HaveDependencyOn "System.Data") |> And <| (ResideInNamespace "ArchTest"))
                 |> Should (ResideInNamespace "SampleLibrary.Data")
                 |> Check

// All the service classes should be sealed
let violations = Types.InCurrentDomain()
                 |> That (ImplementInterface typedefof<IWidgetService>)
                 |> Should (Be Sealed)
                 |> Check
```
