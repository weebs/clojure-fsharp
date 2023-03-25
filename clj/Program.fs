module Clojure.Cli
open System
open System.Text
open FParsec
open FSharp.Data.LiteralProviders
open FSharp.Reflection
open Clojure.ReadEval

let mutable output = ""
// let printfn s = output <- output + "\n" + s

let sb = StringBuilder()
let printfn format =
    // Printf.printfn format
    Printf.bprintf sb format
    // s
    // fprintfn Console.Out format
printfn "Starting program"
let source = __SOURCE_DIRECTORY__ + "/test.clj" |> System.IO.File.ReadAllText
                                        // TextFile.``test.clj``.Path |> System.IO.File.ReadAllText
printfn $"{source.ToString()}"

open Clojure.Parser
match run Parser.file source with
| Success (program, _, _) -> 
    printfn "Parsing succeeded"
    printfn "%A" program
    // let program = program |> List.filter ()
    for token in program do
        match token with
        | Token.Comment comment -> Console.WriteLine(";" + comment)
        | tokenExpr ->
        // match expr witH

        // | Token.Expr expr ->
        // printfn "\nCalling eval with expr %A\n=========================================\n" expr
        // Runtime.eval expr
            tokenExpr
            |> fun s -> Console.WriteLine ((sprintf "%A" s).Replace("\n", " ")); s
            |> Runtime.parseToken
            |> Runtime.evalAst
            |> sprintf "\n                                         ^Evaluation result: %A"
            |> Console.WriteLine
| ope -> printfn "ope! %A" ope
// printfn "%A" (run Clojure.file "; (foo)
// (+ 1 2)")
// Console.WriteLine(sb.ToString())