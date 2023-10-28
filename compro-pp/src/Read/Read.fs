module Read.Read

open System.Collections.Generic
open Syntax

type Type =
    | TTypename of Typename
    | TVec of Type * A

let rec collectDecls =
    function
    | ERead (x, t) -> [ (x, TTypename t) ]
    | ESeq (e1, e2) -> collectDecls e1 @ collectDecls e2
    | ERep (e, _, n) -> List.map (fun (x, t) -> (x, TVec(t, n))) (collectDecls e)

type StringBuilder = System.Text.StringBuilder

let rec genBinary (a1: A) (op: string) (a2: A) = genA a1 + op + genA a2

and genA =
    function
    | AIdent x -> x
    | AInt n -> string n
    | AAdd (a1, a2) -> genBinary a1 " + " a2
    | ASub (a1, a2) -> genBinary a1 " - " a2
    | AMul (a1, a2) -> genBinary a1 " * " a2
    | ADiv (a1, a2) -> genBinary a1 " / " a2
    | AMod (a1, a2) -> genBinary a1 " % " a2
    | APow (a1, a2) -> genBinary a1 " ^ " a2
    | ACode s -> s

let rec genExpr (b: StringBuilder) =
    function
    | ERead (x, _) -> b.AppendFormat("cin >> {0};", x) |> ignore
    | ESeq (e1, e2) ->
        genExpr b e1
        b.Append "\n" |> ignore
        genExpr b e2
    | ERep (e, l, r) ->
        b.AppendFormat("for (int i = {0}; i < {1}; i++) ", genA l, genA r)
        |> ignore

        b.Append("{\n") |> ignore

        genExpr b e
        b.Append("\n}\n") |> ignore

let rec genType (b: StringBuilder) =
    function
    | TTypename t -> t
    | TVec (t, n) -> sprintf "std::vector<%s>" (genType b t)

let generate (e: Expr) =
    let decls = collectDecls e
    let b = StringBuilder()

    for (x, t) in decls do
        b.AppendFormat("{0} {1};\n", genType b t, x)
        |> ignore

    genExpr b e
    b.ToString()

let getReadFunction (source: string) startPos =
    let i = ref startPos

    let read ((arr, x, y): char [] * _ * _) =
        if i.Value >= source.Length then
            0
        else
            arr.[0] <- source.[i.Value]
            i.Value <- i.Value + 1
            1

    (read, i)


let run source i =
    let (read, i) = getReadFunction source i
    let lexbuf = FSharp.Text.Lexing.LexBuffer<char>.FromFunction (read)
    let expr = Parser.expr Lexer.tokenize lexbuf
    (i.Value, generate expr)
