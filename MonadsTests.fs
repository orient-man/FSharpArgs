module MonadsTests
open Monads
open FsUnit
open NUnit.Framework

[<Test>]
let ``all that stuff just works``() =
    let a, s1 = NextShort 0
    let b, s2 = NextShort s1
    let c, _ = NextShort s2
    resultMonadic |> should equal (a + b + c)
    (int16 0, 0)
        |> AggregateRandom
        |> AggregateRandom
        |> AggregateRandom
        |> fst
        |> should equal (a + b + c)
