module Playground
//let rec sum = function
//    | [] -> 0
//    | n::rest -> n + sum rest
let rec foldr f x = function
    | [] -> x
    | n::rest -> f n (foldr f x rest)

let sum = foldr (+) 0
let product = foldr (*) 1
let anytrue = foldr (||) false
let alltrue = foldr (&&) true
let cons x y = x::y
let copy list = foldr cons [] list
let append a b = foldr cons b a
