module FSharpArgs.ArgsTests
open NUnit.Framework
open Args

[<Test>]
let ``Parses with no schema or arguments``() =
    let result = Parse "" Array.empty<string>
    Assert.IsTrue(Map.isEmpty result)