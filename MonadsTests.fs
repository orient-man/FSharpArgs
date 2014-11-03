module MonadsTests
open Monads
open FsUnit
open NUnit.Framework

let expectedResult = 28995

[<Test>]
let ``all that stuff just works``() =
    let a, s1 = NextShort 0
    let b, s2 = NextShort s1
    let c, _ = NextShort s2
    (a + b + c) |> should equal expectedResult
    resultMonadic |> should equal expectedResult
