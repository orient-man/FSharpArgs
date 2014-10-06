module FSharpArgs.ArgsTests
open Args
open FsUnit
open NUnit.Framework

[<Test>]
let ``Parses with no schema or arguments``() =
    let result = Parse "" []
    result.Cordinality |> should equal 0

[<Test>]
let ``Simple bool present``() =
    let result = Parse "x" ["-x"]
    result.GetBool 'x' |> should be True

[<Test>]
let ``Multiple bool arguments``() =
    let result = Parse "xy" ["-xy"]
    result.GetBool 'x' |> should be True
    result.GetBool 'y' |> should be True

[<Test>]
let ``Simple bool not present``() =
    let result = Parse "x" []
    result.GetBool 'x' |> should be False