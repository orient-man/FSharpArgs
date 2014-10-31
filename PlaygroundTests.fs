module PlaygroundTests
open Playground
open FsUnit
open NUnit.Framework

[<Test>]
let ``all that stuff just works``() =
    sum [1;3;4] |> should equal 8
    product [1;3;4] |> should equal 12
    anytrue [false;true;true] |> should be True
    anytrue [false;false] |> should be False
    alltrue [false;true;true] |> should be False
    alltrue [true;true] |> should be True
    copy [1;2;3] |> should equal [1;2;3]
    append [1;2] [3;4] |> should equal [1;2;3;4]
