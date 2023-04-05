module Clojure.Fs.Web.Client.Main

open Elmish
open FSharp.Data.LiteralProviders
open Bolero
open Bolero.Html

type Model = { Source: string; Program: Clojure.Read.Value list; Output: string list; Expr: list<Clojure.Read.Value * Clojure.Read.Value> }

type Message =
    | Compile of string | Println of string //Clojure.Compiler.CompiledValue[]

let runtime = Clojure.Eval.ClojureRuntime()
let update message model =
    match message with
    | Compile text ->
        match FParsec.CharParsers.run Clojure.Read.Parser.file text with
        | FParsec.CharParsers.ParserResult.Success(tokens, _, _) ->
            let expr = tokens |> List.map (fun expr -> expr, runtime.Eval expr)
            { model with Source = text; Program = tokens; Expr = expr }
        | error -> printfn "%A" error; model
    | Println output -> { model with Output = output :: model.Output }

let program = "(println \"1 + 1 = \" (+ 1 1))" // TextFile.``demo.clj``.Text
let mutable source_code = program
let mutable init = false
let view model dispatch =
    if not init then
        init <- true
        runtime.UpdateNamespace("println", Clojure.Read.Value.CompiledFn (fun values ->
            let line = String.concat " " (Array.map string values)
            dispatch <| Println line
            Clojure.Read.Value.Null
        ))
    div {
        attr.style "height: 100%"
        div {
            attr.style "height: 80%"
            table {
                attr.style "height: 100%"
                tr {
                    td {
                        attr.style "width: 50vw"
                        table {
                            for line in model.Output do
                                tr {
                                    td { line }
                                }
                        }
                    }
                    td {
                        attr.style "width: 50vw"
                        table {
                            for (token, result) in model.Expr do
                                tr {
                                    td { code { sprintf "%A" result } }
                                }
                                tr {
                                    td { code { sprintf "%A" token } }
                                }
                        }
                    }
                }
            }
        }
        div {
            attr.style "height: 20%"
            textarea {
                attr.``class`` "textarea"
                bind.input.string source_code (fun str -> source_code <- str)
            }
            div {
                button {
                    attr.``class`` "button is-info"
                    attr.style "width: 100%"
                    on.click (fun _ -> dispatch (Compile source_code))
                    "Eval"
                }
            }
        }
    }
type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let (FParsec.CharParsers.ParserResult.Success (parsed, _, _)) =
            FParsec.CharParsers.run Clojure.Read.Parser.file program
        Program.mkSimple (fun _ -> { Source = program; Program = parsed; Output = []; Expr = [] }) update view
