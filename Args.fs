module FSharpArgs.Args

type ParsingResult(values: Map<char, obj>) =
    let getValue arg convert defaultValue =
        match values |> Map.tryFind arg with
        | Some(v) -> convert v
        | None -> defaultValue

    member this.Cordinality = values.Count
    member this.GetBool arg = getValue arg System.Convert.ToBoolean false
    member this.GetString arg = getValue arg (fun v -> v.ToString()) ""

let BoolMarshaler args = (box true, args)
let StringMarshaler = function | [] -> (box "", []) | (head::tail) -> (box head, tail)

let parse (schema: string) args =
    let parseSchema =
        let marshalers =
            let parseSchemaElement = function
                | (c, "*") -> (c, StringMarshaler)
                | (c, _) -> (c, BoolMarshaler)

            schema.Split ','
            |> Seq.map (fun s -> s.Trim())
            |> Seq.filter (fun s -> s.Length > 0)
            |> Seq.map (fun s -> (s.[0], s.Substring(1)))
            |> Seq.map parseSchemaElement
            |> Map.ofSeq

        fun arg -> marshalers |> Map.find arg

    let parseArguments findMarshaler =
        let rec parseArguments (values, args) =
            let (|ValidArgument|_|) arg =
                match List.ofSeq arg with | ('-'::c::_) -> Some(c) | _ -> None

            let parseArgument = function
                | (ValidArgument c::args) ->
                    let (value, args) = args |> findMarshaler c
                    Some((c, value), args)
                | _ -> None

            let append = function
                | Some((value, args)) -> (value::values, args)
                | None -> (values, args |> List.tail)

            match args with
            | [] -> values
            | _ -> parseArgument args |> append |> parseArguments

        ParsingResult(parseArguments ([], args) |> Map.ofSeq)

    parseArguments parseSchema 