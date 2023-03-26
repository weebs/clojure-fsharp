module Clojure.Cli
open Parser
open System
open System.Text
open System.Threading
open FParsec

let mutable output = ""

// let sb = StringBuilder()
// let printfn format =
//     // Printf.printfn format
//     Printf.bprintf sb format
//    s
//    fprintfn Console.Out format
    
printfn "Starting program"
open FParsec
let rt =  Compiler.ClojureRuntime ()
printfn "Starting F# clojure"
let mutable inputHistory = []
// Environment variables here are a hack to communicate between threads
// in the F# interactive session, so a new instance can say "gtfo" :)
if Environment.GetEnvironmentVariable("clojure_running") = "true" then
    Environment.SetEnvironmentVariable("clojure_running", "stop_requested")
    while Environment.GetEnvironmentVariable("clojure_running") = "stop_requested" do
        printfn "waiting"; Thread.Sleep(100)
task {
    Environment.SetEnvironmentVariable("clojure_running", "true")
    Console.Write("clojure> ")
    let newline = char "\013" // \r
    
    let mutable input = ""
    let mutable index = -1
    
    let reprintPrompt () =
        Console.CursorLeft <- 0
        Console.Write "clojure> "
        printf "%s" input
        let n = Console.CursorLeft
        for i in n..(Console.BufferWidth - n) do
            printf " "
        Console.CursorLeft <- n
    while Environment.GetEnvironmentVariable("clojure_running") = "true" do
        if Console.KeyAvailable then
            match Console.ReadKey() with
            | key when key.KeyChar = newline ->
                let line = input
                input <- ""
                printf "\n"
            // while line <> "q" do
                inputHistory <- line :: inputHistory
                index <- -1
                try
                    match run Parser.value line with
                    | ParserResult.Success(expr, _, _) ->
                        rt.Eval expr
                        |> function
                        // | Compiler.Value value -> printfn "%A" value
                        | value -> printfn "%A" value
                    | error -> printfn "%A" error
                with runtimeException -> printfn "%A" runtimeException
                Console.Write("clojure> ")
                // line <- Console.ReadLine()
            | c when c.Key = ConsoleKey.DownArrow && index > 0 ->
                index <- index - 1
                input <- inputHistory[index]
                reprintPrompt ()
            | c when c.Key = ConsoleKey.UpArrow && inputHistory.Length > index + 1 ->
                index <- index + 1
                input <- inputHistory[index]
                reprintPrompt ()
            | c when c.Key = ConsoleKey.Backspace ->
                if input.Length > 0 then
                    input <- input.Substring(0,input.Length - 1)
                reprintPrompt ()
            | c when Char.IsControl(c.KeyChar) = false ->
                // printfn "char = %A" c.Key
                input <- input + string c.KeyChar
            | _ -> reprintPrompt ()
        else
            do! Async.Sleep 100
    
    Environment.SetEnvironmentVariable("clojure_running", "false")
} |> fun t ->
    #if !INTERACTIVE
    t.Wait()
    #else
    ()
    #endif