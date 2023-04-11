module Clojure.Cli
open System.IO
open Clojure.Read
open System
open System.Text
open System.Threading

let mutable output = ""

// let sb = StringBuilder()
// let printfn format =
//     // Printf.printfn format
//     Printf.bprintf sb format
//    s
//    fprintfn Console.Out format
// Not sure why I'm having issues with Thoth.Json in F# interactive :(
let serializeList (items: string list) =
    String.concat ";;\n" items
let deserializeList (text: string) =
    text.Split(";;\n") |> Array.toList

let _newline = char "\013" // \r
type Buffer = { contents: string; cursor: int; }
    with
    member this.Update(key: ConsoleKeyInfo) =
        match key with
        | c when c.Key = ConsoleKey.Backspace &&
                 this.contents.Length > 0 &&
                 this.cursor > 0 ->
            { this with
                contents =
                    this.contents.Substring(0, this.cursor - 1) +
                    this.contents.Substring(this.cursor, this.contents.Length - this.cursor)
                cursor = this.cursor - 1 }
        | c when c.Key = ConsoleKey.Home -> { this with cursor = 0 }
        | c when c.Key = ConsoleKey.LeftArrow && this.cursor > 0 ->
            { this with cursor = this.cursor - 1 }
        | c when c.Key = ConsoleKey.RightArrow && this.cursor < this.contents.Length ->
            { this with cursor = this.cursor + 1 }
        | c when Char.IsControl(c.KeyChar) = false ->
            { this with
                contents =
                    this.contents.Substring(0, this.cursor) +
                    (string c.KeyChar) + this.contents.Substring(this.cursor, this.contents.Length - this.cursor)
                cursor = this.cursor + 1  }
        | c ->
            match c.Key with
            | ConsoleKey.End -> { this with cursor = this.contents.Length } | _ -> this
            
type ReplBuffer = { current: Buffer; History: string list }
printfn "Starting program"
open FParsec
let rt = Eval.ClojureRuntime ()
printfn "Starting F# clojure"
let mutable inputHistory = if IO.File.Exists("history.json") then deserializeList (IO.File.ReadAllText "history.json") else  []
let fsiConfig = FSharp.Compiler.Interactive.Shell.FsiEvaluationSession.GetDefaultConfiguration ()
let sbOut = new StringBuilder()
let sbErr = new StringBuilder()
let inStream = new StringReader("")
let outStream = new StringWriter(sbOut)
let errStream = new StringWriter(sbErr)
// fsi.EvalInteraction "eval \"1234\""
// fsi.EvalInteraction "(eval :?> string -> unit) \"System.Console.WriteLine(\"yo\")\""
// fsi.EvalInteraction "printfn \"%A\" <| _FSI.EvalInteraction.GetType()"
// fsi.EvalInteraction "printfn \"%A\" (_FSI.EvalInteraction (\"printfn \"hi\"\"))"

// Environment variables here are a hack to communicate between threads
// in the F# interactive session, so a new instance can tell the old one to exit
if Environment.GetEnvironmentVariable("clojure_running") = "true" then
    Environment.SetEnvironmentVariable("clojure_running", "stop_requested")
    while Environment.GetEnvironmentVariable("clojure_running") = "stop_requested" do
        printfn "waiting"; Thread.Sleep(100)
task {
    Environment.SetEnvironmentVariable("clojure_running", "true")
    let fsi = FSharp.Compiler.Interactive.Shell.FsiEvaluationSession.Create (fsiConfig, [| ""; "--multiemit-" |], inStream, outStream, errStream)
    fsi.AddBoundValue ("_FSI", fsi)
    fsi.AddBoundValue("eval", box (fun (s: string) -> fsi.EvalInteraction s))
    // fsi.EvalInteraction "FSI.EvalInteraction \"printfn \"1234 i declare a thumb war\"\""
    fsi.EvalInteraction """printfn "%A" <| eval.GetType().GetMethod("Invoke").Invoke(eval, [| "printfn \"%d\" 1234" |])"""
    fsi.EvalInteraction """printfn "%A" <| _FSI.GetType().GetMethod("EvalInteraction").Invoke(_FSI, [| "printfn \"%d\" 1234"; null |])"""
    rt.UpdateNamespace("fsi", Value.Obj fsi)
    Console.Write("clojure> ")
    
    let mutable buffer = { contents = ""; cursor = 0 }
    let mutable index = -1
    let reprintPrompt () =
        Console.CursorLeft <- 0
        Console.Write "clojure> "
        printf "%s" buffer.contents
        let n = Console.CursorLeft
        for i in n..(Console.BufferWidth - n) do
            printf " "
        // TODO handle inputs that span multiple lines
        Console.CursorLeft <- n - (buffer.contents.Length - buffer.cursor)
    
    while Environment.GetEnvironmentVariable("clojure_running") = "true" do
        if Console.KeyAvailable then
            match Console.ReadKey() with
            | key when key.KeyChar = _newline ->
                let line = buffer.contents
                printf "\n"
                inputHistory <- line :: inputHistory
                IO.File.WriteAllText ("history.json", serializeList inputHistory)
                index <- -1
                try
                    match run Parser.value line with
                    | ParserResult.Success(expr, _, _) ->
                        rt.Eval expr
                        |> function
                        | value -> printfn "%A" value
                    | error -> printfn "%A" error
                with runtimeException -> printfn "%A" runtimeException
                Console.Write("clojure> ")
                buffer <- { contents = ""; cursor = 0 }
            | c when c.Key = ConsoleKey.DownArrow && index = 0 ->
                index <- -1
                buffer <- { contents = ""; cursor = 0 }
                reprintPrompt ()
            | c when c.Key = ConsoleKey.DownArrow && index > 0 ->
                index <- index - 1
                buffer <- { contents = inputHistory[index]; cursor = inputHistory[index].Length }
                reprintPrompt ()
            | c when c.Key = ConsoleKey.UpArrow && inputHistory.Length > index + 1 ->
                index <- index + 1
                buffer <- { contents = inputHistory[index]; cursor = inputHistory[index].Length }
                reprintPrompt ()
            | c ->
                buffer <- buffer.Update(c)
                reprintPrompt ()
        else
            do! Async.Sleep 100
    
    Environment.SetEnvironmentVariable("clojure_running", "false")
} |> fun t ->
    #if !INTERACTIVE
    t.Wait()
    #else
    ()
    #endif