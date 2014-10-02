module FSharpArgs.Args

let Parse schema args =
    Map.empty<char, obj>