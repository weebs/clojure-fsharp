module Clojure.Source
open System.Text

let getRange (lines: string[]) (start: FParsec.Position) (_end: FParsec.Position) =
  // printfn "Get Range: %A %A" start _end
  let sb = StringBuilder()
  sb.AppendLine(lines.[int start.Line - 1].Substring(int start.Column - 1, if _end.Line > start.Line then lines.[int start.Line - 1].Length - int start.Column else int (System.Math.Max(0L, _end.Column - start.Column))))
  for i in (int start.Line)..(int _end.Line - 2) do
    sb.AppendLine(lines.[i])
  if _end.Line > start.Line then
    sb.AppendLine(lines.[int _end.Line - 1].Substring(0, int _end.Column - 1))
    |> ignore
  sb.ToString()
let filewatch filepath callback cancellation =
  printfn "Starting file watch"
  async {
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
          async { callback filepath s t } |> Async.Start
    printfn "Exiting task"
  } |> Async.StartAsTask

