module Playground
open FsUnit
open NUnit.Framework

//let rec sum = function
//    | [] -> 0
//    | n::rest -> n + sum rest

let rec foldr f x = function
    | [] -> x
    | n::rest -> f n (foldr f x rest)

let sum = foldr (+) 0

[<Test>]
let ``sum just works``() =
    sum [1;3;4] |> should equal 8

let product = foldr (*) 1

[<Test>]
let ``product just works``() =
    product [1;3;4] |> should equal 12

let anytrue = foldr (||) false

[<Test>]
let ``anytrue just works``() =
    anytrue [false;true;true] |> should be True
    anytrue [false;false] |> should be False

let alltrue = foldr (&&) true

[<Test>]
let ``alltrue just works``() =
    alltrue [false;true;true] |> should be False
    alltrue [true;true] |> should be True

let cons x y = x::y
let copy list = foldr cons [] list

[<Test>]
let ``copy just works``() =
    copy [1;2;3] |> should equal [1;2;3]

let append a b = foldr cons b a

[<Test>]
let ``append just works``() =
    append [1;2] [3;4] |> should equal [1;2;3;4]
