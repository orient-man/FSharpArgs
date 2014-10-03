module FSharpArgs.ArgsTests
open NUnit.Framework
open Args

[<Test>]
let ``Parses with no schema or arguments``() =
    let result = Parse "" []
    Assert.AreEqual(0, result.Cordinality)

[<Test>]
let ``Simple bool present``() =
    let result = Parse "x" ["-x"]
    Assert.IsTrue(result.GetBool 'x')

[<Test>]
let ``Multiple bool arguments``() =
    let result = Parse "xy" ["-xy"]
    Assert.IsTrue(result.GetBool 'x')
    Assert.IsTrue(result.GetBool 'y')

[<Test>]
let ``Simple bool not present``() =
    let result = Parse "x" []
    Assert.IsFalse(result.GetBool 'x')