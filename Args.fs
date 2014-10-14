module FSharpArgs.Args

type ParsingResult(values) =
    let getValue arg (convert: obj -> 'a) defaultValue =
        match values |> Map.tryFind arg with
        | Some(v) -> convert v
        | None -> defaultValue

    member this.Cordinality = values.Count
    member this.GetBool arg = getValue arg System.Convert.ToBoolean false
    member this.GetString arg = getValue arg (fun v -> v.ToString()) ""

let BoolMarshaler args = (box true, args)
let StringMarshaler = function | [] -> (box "", []) | (head::tail) -> (box head, tail)

let parseSchema (schema: string) =
    let parseSchemaElement = function
        | (c, "*") -> (c, StringMarshaler)
        | (c, _) -> (c, BoolMarshaler)

    schema.Split ','
    |> Seq.map (fun s -> s.Trim())
    |> Seq.filter (fun s -> s.Length > 0)
    |> Seq.map (fun s -> (s.[0], s.Substring(1)))
    |> Seq.map parseSchemaElement
    |> Map.ofSeq

let parse (schema: string) args =
    let marshalers = parseSchema schema

    let (|ValidArgument|_|) arg =
        match List.ofSeq arg with | ('-'::c::_) -> Some(c) | _ -> None

    let parseArgument = function
        | (ValidArgument c::args) ->
            let (value, args) = marshalers.[c] args
            Some((c, value), args)
        | _ -> None

    let appendParsingResult (values, args) = function
        | Some((value, args)) -> (value::values, args)
        | None -> (values, args |> List.tail)

    let rec parseArguments current =
        match current with
        | (values, []) -> values
        | (values, args) ->
            parseArgument args |> appendParsingResult current |> parseArguments

    let values = parseArguments ([], args) |> Map.ofSeq

    ParsingResult values
