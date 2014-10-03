module FSharpArgs.Args

type ParsingResult(valuesIn: Map<char, obj>) =
    let values = valuesIn
    member this.Cordinality = values.Count
    member this.GetBool arg =
        match values.TryFind arg with
            | Some(v) -> System.Convert.ToBoolean(v)
            | None -> false

let Parse schema args =
    let validArgs = schema |> Set.ofSeq

    let FindElements (arg: string) =
        if arg.StartsWith "-"
        then arg |> Seq.skip 1
        else Seq.empty

    let values =
        args
        |> Seq.ofList
        |> Seq.map FindElements
        |> Seq.concat
        |> Seq.where validArgs.Contains
        |> Seq.map (fun c -> (c, box true))
        |> Map.ofSeq

    ParsingResult values