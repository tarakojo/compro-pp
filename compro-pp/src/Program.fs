open System
open System.IO

type StringBuilder = System.Text.StringBuilder

let builder = StringBuilder()

(*
コマンドライン引数をパースする
*)

type Args =
    { outPath: string option
      headerPath: string option
      sourcePath: string }

let args =

    let mutable outPath = None
    let mutable headerPath = None
    let mutable sourcePath = None

    let rec parse args =
        match args with
        | "-hd" :: _ when headerPath.IsSome -> failwith "Header path was specified twice"
        | "-hd" :: headerValue :: tail ->
            headerPath <- Some headerValue
            parse tail
        | "-o" :: _ when outPath.IsSome -> failwith "Output path was specified twice"
        | "-o" :: outValue :: tail ->
            outPath <- Some outValue
            parse tail
        | pathValue :: _ when sourcePath.IsSome -> failwith "Source path was specified twice"
        | pathValue :: tail ->
            sourcePath <- Some pathValue
            parse tail
        | [] -> ()

    System.Environment.GetCommandLineArgs()
    |> List.ofArray
    |> List.tail
    |> parse

    if sourcePath.IsNone then
        failwith "Source path was not specified"

    { outPath = outPath
      headerPath = headerPath
      sourcePath = sourcePath.Value }

printfn "Source: %s" args.sourcePath

(*
ヘッダーを読み込み
*)
if args.headerPath.IsSome then
    let headerPath = args.headerPath.Value
    printfn "Header : %s" headerPath

    if File.Exists(headerPath) then
        use streamReader = new StreamReader(headerPath)
        builder.Append(streamReader.ReadToEnd()) |> ignore
        builder.Append "\n\n" |> ignore
    else
        failwith
        <| sprintf "File not found at path: %s" headerPath
else
    printfn "Header : None"

(*
ソースを読む
*)
let source =
    try
        File.ReadAllText(args.sourcePath)
    with
    | ex -> failwith (sprintf "could not read source file %s" args.sourcePath)

(* 識別子を読む関数 *)
let readIdentifier (i: int) =
    let rec read (i: int) (acc: string) =
        match source.[i] with
        | c when
            ('a' <= c && c <= 'z')
            || ('A' <= c && c <= 'Z')
            || ('0' <= c && c <= '9')
            || (c = '_')
            ->
            read (i + 1) (acc + string source.[i])
        | _ when acc.Length = 0 -> failwith "Invalid identifier"
        | _ -> (i, acc)

    read i ""

let runpp identifier source i =
    match identifier with
    | "R" -> Read.Read.run source i
    | _ ->
        failwith
        <| sprintf "Unknown preprocess : %s" identifier

let rec read =
    function
    | i when i >= source.Length -> ()
    | i when source.[i] = '$' ->
        let (i, identifier) = readIdentifier (i + 1)
        let (i, code) = runpp identifier source i
        builder.Append(code) |> ignore
        read i
    | i ->
        builder.Append(source.[i]) |> ignore
        read (i + 1)

read 0

(*出力*) 
let outPath =
    match args.outPath with
    | Some path -> path
    | None ->
        Path.GetFileNameWithoutExtension(args.sourcePath)
        + "_result.cpp"

printfn "Output: %s" outPath
File.WriteAllText(outPath, builder.ToString())
printfn "Done!"
