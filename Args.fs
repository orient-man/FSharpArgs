module FSharpArgs.Args

type ParsingResult(valuesIn: Map<char, obj>) =
    let values = valuesIn
    member this.Cordinality = values.Count
    member this.GetBool arg =
        match values.TryFind arg with
            | Some(v) -> System.Convert.ToBoolean(v)
            | None -> false

let Parse (schema:string) args =
    let values =
        schema
        |> Seq.map (fun s -> (s, box true))
        |> Map.ofSeq

    new ParsingResult(values)