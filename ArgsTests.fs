module FSharpArgs.ArgsTests
open NUnit.Framework
open Args

[<Test>]
let ``Parses with no schema or arguments``() =
    let result = Parse "" Array.empty<string>
    Assert.AreEqual(0, result.Cordinality)

[<Test>]
let ``Simple bool present``() =
    let result = Parse "x" ["-x"]
    Assert.IsTrue(result.GetBool 'x')