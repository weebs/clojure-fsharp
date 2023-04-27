module Clojure.UnitTests
open System
open System.Reflection
open System.Text
open Clojure.Eval
open DI



let rt = ClojureRuntime()

let run testCode =
  try
    match FParsec.CharParsers.run Read.Parser.file testCode with
    | FParsec.CharParsers.ParserResult.Success(expressions, _, _) ->
      let lines = testCode.Split "\n"
      for ((start, expr), srcPos) in expressions do
        let range = Source.getRange lines start srcPos
        // printfn "Start: %A" start
        // printfn "%A: %s" srcPos <| lines[int srcPos.Line - 1].Substring(int srcPos.Column - 1)
        // printfn "%A to %A\n%s" start srcPos range
        
        let struct (col, row) = Console.GetCursorPosition()
        // Console.Clear()
        // Console.CursorTop <- 0
        
        // Only print the first 5 lines of the source
        for (index, line) in ((range.Split("\n") |> Array.mapi (fun i a -> i, a) |> fun a -> a |> Array.take (a.Length - 1)) |> fun a -> a |> Array.take (Math.Min(5, a.Length))) do
          let s = sprintf "%d: %s" (start.Line + int64 index) line
          // Console.CursorLeft <- Console.WindowWidth - s.Length
          printfn "%s" s
          // Console.CursorTop <- Console.CursorTop + 1
        // Console.SetCursorPosition (col, row)
        printfn "=> %A" <| rt.Eval expr
      | error ->
        printfn $"%A{error}"; printfn ""
  with error -> printfn "%A" error
let watchUnitTestFile fileName =
  let codePath = IO.Path.Join(__SOURCE_DIRECTORY__, fileName) // "/workinprogress.clj")
  let testCode = System.IO.File.ReadAllText codePath
  try
    let t = Dependency.repl <| fun () ->
      let token = ref false
      { cancel = token; task = (Source.filewatch codePath (fun _ contents _ -> run contents) token) }
    match t with
    | Some t -> printfn "cancelling task"; t.cancel.contents <- true
    | _ -> printfn "couldn't find repl item"
    run testCode
  with error -> printfn "%A" error
  
// watchUnitTestFile "console.clj"
    
// Read.Dlr.dlrTest ()

// let rt = ClojureRuntime()
try
    rt.Eval $"(println (+ 1 1))"
    |> printfn "%A"
    // printfn "%A" <| rt.Cache["System.Console"].GetType("System.Console").GetMethods()
    printfn "%A" (rt.Types["System.Console"].GetMethods())// |> Array.filter (fun t -> t.Name = "System.Console")
    rt.Eval "(System.Console/WriteLine 1234)" |> printfn "%A"
    // rt.Eval """(do (println "Reading Line:" (System.Console/ReadLine)) (println "yo")) """ |> printfn "%A"
    
    rt.Eval "(println (. 12345 ToString))" |> printfn "%A"
    // rt.Eval "(println (.ToString 12345))" |> printfn "%A"
    
    rt.Eval("(types)")
    |> printfn "%A"
    rt.Eval("(. (types) GetType)")
    |> printfn "%A"
    rt.Eval """
;(println (types))
(println (. (. types GetType) ToString)) ; (. (types) GetType)))
""" |> ignore
with error -> printfn "%A" error