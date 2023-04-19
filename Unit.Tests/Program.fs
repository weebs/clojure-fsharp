module Clojure.UnitTests
open System.Text
open Clojure.Eval
open DI
open FSharp.Data.LiteralProviders
let filewatch filepath callback cancellation =
  printfn "Starting file watch"
  task {
    let mutable contents = System.IO.File.ReadAllText filepath
    let mutable changed = System.IO.File.GetLastWriteTime filepath
    
    while cancellation.contents = false do
      do! Async.Sleep 100
      // printfn "cancellation = %A" cancellation
      let t = System.IO.File.GetLastWriteTime filepath 
      if changed <> t then
        changed <- t
        let s = System.IO.File.ReadAllText filepath
        if contents <> s then
          contents <- s
          task { callback filepath s t } |> ignore
    printfn "Exiting task"
  }



let rt = ClojureRuntime()
let codePath = (__SOURCE_DIRECTORY__ + "/raymarcher.clj")
let testCode = System.IO.File.ReadAllText codePath

let getRange (lines: string[]) (start: FParsec.Position) (_end: FParsec.Position) =
  // printfn "Get Range: %A %A" start _end
  let sb = StringBuilder()
  sb.AppendLine(lines[int start.Line - 1].Substring(int start.Column - 1, if _end.Line > start.Line then lines[int start.Line - 1].Length - int start.Column else int (System.Math.Max(0L, _end.Column - start.Column))))
  for i in (int start.Line)..(int _end.Line - 2) do
    printfn "appending line %d" i
    sb.AppendLine(lines[i])
  if _end.Line > start.Line then
    sb.AppendLine(lines[int _end.Line - 1].Substring(0, int _end.Column - 1))
    |> ignore
  sb.ToString()
  
let run testCode =
  try
    match FParsec.CharParsers.run Read.Parser.file testCode with
    | FParsec.CharParsers.ParserResult.Success(expressions, _, _) ->
      let lines = testCode.Split "\n"
      for ((start, expr), srcPos) in expressions do
        let range = getRange lines start srcPos
        // printfn "Start: %A" start
        // printfn "%A: %s" srcPos <| lines[int srcPos.Line - 1].Substring(int srcPos.Column - 1)
        // printfn "%A to %A\n%s" start srcPos range
        
        for (index, line) in (range.Split("\n") |> Array.mapi (fun i a -> i, a) |> fun a -> a |> Array.take (a.Length - 1)) do
          printfn "%d: %s" (start.Line + int64 index) line
        printfn "=> %A" <| rt.Eval expr
        // printfn "\n"
        // printfn "yo"
        ()
        
      | _else -> printfn "%A" _else
  with error -> printfn "%A" error
try
  let t = Dependency.repl <| fun () ->
    let token = ref false
    { cancel = token; task = (filewatch codePath (fun _ contents _ -> run contents) token) }
  match t with
  | Some t -> printfn "cancelling task"; t.cancel.contents <- true
  | _ -> printfn "couldn't find repl item"
  run testCode
with error -> printfn "%A" error
