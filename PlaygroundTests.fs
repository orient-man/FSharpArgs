﻿module PlaygroundTests
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
    length [1;2;3] |> should equal 3
    doubleall [1;2;3] |> should equal [2;4;6]
    summatrix [[1;2];[3;4]] |> should equal 10
    sumtree (Node(1, [Node(2, []); Node(3, [Node(4, [])])])) |> should equal 10
    sumtree (Node(1, Node(2, [])::Node(3, Node(4, [])::[])::[])) |> should equal 10
