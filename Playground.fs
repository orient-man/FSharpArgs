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
let count a n = n + 1
let length list = foldr count 0 list
let double = (*) 2
//let fandcons f = f >> cons
//let doubleandcons = fandcons double
//let doubleall = foldr doubleandcons []
//let doubleall = foldr (double >> cons) []
let map f = foldr (f >> cons) []
let doubleall = map double
let summatrix = map sum >> sum

type Tree<'a> = | Node of 'a * Tree<'a> list

let rec foldtree f g a = function
    | Node(label, subtrees) ->
        f label (foldchildren f g a subtrees)
and foldchildren f g a = function
    | [] -> a
    | subtree::rest ->
        g (foldtree f g a subtree) (foldchildren f g a rest)

let sumtree = foldtree (+) (+) 0