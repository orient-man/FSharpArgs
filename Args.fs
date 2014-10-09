module FSharpArgs.Args

type ParsingResult(values: Map<char, obj>) =
    let getValue arg convert defaultValue =
        match values.TryFind arg with
        | Some(v) -> convert v
        | None -> defaultValue

    member this.Cordinality = values.Count
    member this.GetBool arg = getValue arg System.Convert.ToBoolean false
    member this.GetString arg = getValue arg (fun v -> v.ToString()) ""

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
