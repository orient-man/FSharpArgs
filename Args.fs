module FSharpArgs.Args

type ParsingResult(valuesIn: Map<char, obj>) =
    let values = valuesIn
    member this.Cordinality = values.Count

    member this.GetBool arg =
        match values.TryFind arg with
        | Some(v) -> System.Convert.ToBoolean(v)
        | None -> false

    member this.GetString arg =
        match values.TryFind arg with
        | Some(v) -> v.ToString()
        | None -> ""

let ParseSchema (schema: string) =
    let BoolMarshaler args = (box true, args)

    let StringMarshaler args =
        (box (args |> Seq.head), args |> Seq.skip 1)

    let parseSchemaElement element =
        match element with
        | (c, "*") -> (c, StringMarshaler)
        | (c, _) -> (c, BoolMarshaler)

    schema.Split ','
    |> Seq.map (fun s -> s.Trim())
    |> Seq.filter (fun s -> s.Length > 0)
    |> Seq.map (fun s -> (s.[0], s.Substring(1)))
    |> Seq.map parseSchemaElement
    |> Map.ofSeq

let Parse (schema: string) args =
    let marshalers = ParseSchema schema

    let parseArgument args =
        let firstArg: string = args |> Seq.head
        if firstArg.StartsWith "-"
        then
            let c = firstArg |> Seq.skip 1 |> Seq.head
            let args = args |> Seq.skip 1
            let (value, args) = marshalers.[c] args
            Some((c, value), args)
        else
            None

    let appendParsingResult current result =
        let (values, args) = current
        match result with
        | Some((value, args)) ->
            (values |> Seq.append [value], args)
        | None -> (values, args |> Seq.skip 1)

    let rec parseArguments current =
        let (values, args) = current
        if args |> Seq.isEmpty
        then values
        else
            parseArgument args
            |> appendParsingResult current
            |> parseArguments

    let values = parseArguments (Seq.empty, args) |> Map.ofSeq

    ParsingResult values
