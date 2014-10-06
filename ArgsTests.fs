module FSharpArgsTests.ArgsTests
open FSharpArgs.Args
open FsUnit
open NUnit.Framework

[<Test>]
let ``Parses with no schema or arguments``() =
    let result = Parse "" []
    result.Cordinality |> should equal 0

[<Test>]
let ``Bool argument present``() =
    let result = Parse "x" ["-x"]
    result.GetBool 'x' |> should be True

// TODO:
//[<Test>]
//let ``Multiple bool arguments``() =
//    let result = Parse "x,y" ["-xy"]
//    result.GetBool 'x' |> should be True
//    result.GetBool 'y' |> should be True

[<Test>]
let ``Bool argument not present``() =
    let result = Parse "x" []
    result.GetBool 'x' |> should be False

[<Test>]
let ``String argument present``() =
    let result = Parse "x*" ["-x"; "param"]
    result.GetString 'x' |> should equal "param"

[<Test>]
let ``String argument not present``() =
    let result = Parse "x*" []
    result.GetString 'x' |> should equal ""

[<Test>]
let ``Mixed type arguments present``() =
    let result = Parse "x*,y,z*" ["-x"; "s1"; "-y"; "-z"; "s2"]
    result.GetString 'x' |> should equal "s1"
    result.GetBool 'y' |> should be True
    result.GetString 'z' |> should equal "s2"