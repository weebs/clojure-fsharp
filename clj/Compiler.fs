module Clojure.Compiler
open Parser

type Runtime<'value, 'expr> =
    // Macro expansion occurs here
    abstract member compile: 'value -> 'expr
    abstract member eval: 'value -> 'expr
    // abstract member parse: Value -> Value
    abstract member readString: string -> 'value
    
type CompiledValue = Value

type Scope = Map<string, CompiledValue>
type Name = string
type ClojureRuntime () =
    let mutable namespaces: Map<Name, Scope ref> = Map.empty
    let mutable locals: Scope list = []
    let mutable ns = Map.empty
    let rec eval item : CompiledValue =
        match item with
        // todo: compile item => evalCompiled
        | Value.TokenList tokens when tokens.Length > 0 ->
            match tokens[0] with
            | Value.Symbol name ->
                match ns.TryFind name with
                | Some (Value.MacroDefn defn) ->
                    eval (defn (Array.tail tokens))
                | Some (Value.CompiledFn defn) ->
                    // todo: resolve Symbols before call
                    defn (Array.tail tokens |> Array.map eval)
                | Some (Value.Builtin form) ->
                    form (Array.skip 1 tokens)
                | Some value -> value
                | _ ->
                    failwithf "Token %A is not callable" tokens[0]
            | _ -> 
                failwithf "Token %A is not callable" tokens[0]
        | Value.Symbol name ->
            locals
            |> List.tryPick (fun items -> items |> Map.tryFind name)
            |> Option.defaultWith (fun () -> ns[name])
            // |> function (Value i) -> i
        | item -> item
    let parseLetBindings (exprs: Value[]) =
        let (Value.Vector bindings) = exprs[0]
        let names = bindings |> Array.chunkBySize 2 |> Array.map Array.head |> Array.map (fun (Value.Symbol name) -> name)
        let values = bindings |> Array.chunkBySize 2 |> Array.map (fun a -> a[1]) |> Array.map eval
        Map.ofArray (Array.zip names values)
    let parseMacroBindings (exprs: Value[]) =
        let (Value.Vector bindings) = exprs[0]
        let names = bindings |> Array.chunkBySize 2 |> Array.map Array.head |> Array.map (fun (Value.Symbol name) -> name)
        let values = bindings |> Array.chunkBySize 2 |> Array.map (fun a -> a[1])
        Map.ofArray (Array.zip names values)
    do
        ns <- ns.Add("let", Value.Builtin (fun values ->
            locals <- parseLetBindings values :: locals
            let mutable result = Value.Null
            for expr in values do
                result <- eval expr
            locals <- List.tail locals
            result
        ))
        ns <- ns.Add("if", Value.Builtin (fun values ->
            match values[0] with
            | Value.Boolean true -> eval values[1]
            | _ -> eval values[2]
        ))
        ns <- ns.Add("defn", Value.MacroDefn (fun values ->
            let (Value.Symbol name) = values[0]
            let (Value.Vector args) = values[1]
            ns <- ns.Add(name, Value.CompiledFn (fun fnValues ->
                let argsMap =
                    match args |> Array.tryFindIndex (function Value.Symbol "&" -> true | _ -> false) with
                    | Some i ->
                        let args = Array.append (Array.take i args) (Array.skip (i + 1) args) |> Array.map (function Value.Symbol name -> name)
                        let values = Array.append (Array.take i fnValues) [| (Value.Vector (Array.skip (i + 1) fnValues)) |]
                        Map.ofArray (Array.zip args values)
                    | None -> Map.ofArray (Array.zip (args |> Array.map (function Value.Symbol name -> name)) fnValues)
                let mutable result = Value.Null
                locals <- argsMap :: locals
                for expr in (Array.skip 2 values) do
                    result <- eval expr
                locals <- List.tail locals
                result
            ))
            Value.Null
        ))
        ns <- ns.Add("+", Value.CompiledFn (fun values ->
            let mutable count_int = 0L
            let mutable count = 0.0
            let mutable anyFloats = false
            for value in values do
                match value with
                | (Value.Number i) when anyFloats -> count <- count + double i
                | (Value.Number i) when not anyFloats -> count_int <- count_int + i
                | (Value.Float f) ->
                    if not anyFloats then
                        anyFloats <- true
                        count <- double count_int
                    count <- count + f
                | _ -> ()
            if not anyFloats then
                Value.Number count_int
            else
                Value.Float count
        ))
        ns <- ns.Add("infix", Value.MacroDefn (fun tokens ->
            match tokens[0] with
            | Value.TokenList tokens ->
                Value.TokenList [| tokens[1]; tokens[0]; yield! (Array.skip 2 tokens) |]
            | _ -> failwithf "Invalid args for infix: %A" tokens
        ))
        ns <- ns.Add("println", Value.CompiledFn (fun values ->
            printfn "%A" values
            Value.Null
        ))
        ns <- ns.Add("defmacro", Value.MacroDefn (fun tokens ->
            match tokens[0] with
            | Value.Symbol name ->
                ns <- ns.Add(name, Value.MacroDefn (fun items ->
                    let (Value.Vector macroArgumentNames) = tokens[1]
                    let macroBindings = Map.ofArray <| Array.zip (Array.map (function Value.Symbol name -> name) macroArgumentNames) items
                    // locals <- (macroBindings |> Map.map (fun name value -> value)) :: locals
                    locals <- macroBindings :: locals
                    for i in 2..(tokens.Length - 2) do
                        eval tokens[i]
                        |> ignore
                    let rec loop expr =
                        match expr with
                        | Value.Quote item -> loop item
                        | Value.QuoteSyntax item -> loop item
                        | Value.TokenList items ->
                            // todo: Handle ~@
                            Value.TokenList (Array.map loop items)
                        | Value.Unquote (Value.Symbol item) ->
                            macroBindings[item]
                        | Value.Unquote item -> eval item
                            // match eval item with
                            // | Value value -> value
                            // | value -> failwithf "Unexpected evaluation term result in macro expasion:\n%A" value
                        | item -> item
                        | _ -> failwithf "Unexpected token: %A" expr
                    let result = loop tokens[tokens.Length - 1]
                    locals <- List.tail locals
                    result
                ))
            | value -> failwithf "Error: given %A for symbol name" value
            Value.Null
        ))
    do
        ns <- ns.Add("-", CompiledFn clojure.core.Math.subtract)
    member this.Eval = eval
    member this.UpdateNamespace(name, value) =
        ns <- ns.Add(name, value)
    //{ new Runtime<Value.Token, CompiledValue> with
    interface Runtime<Value, CompiledValue> with
        member this.compile token = token
        member this.eval item = eval item
        member this.readString text = FParsec.CharParsers.run Parser.expr text |> function FParsec.CharParsers.ParserResult.Success (token,_,_) -> token

    
[<Xunit.Fact>]
let run_demo () =
    match FParsec.CharParsers.run Parser.value "(infix (1 + 1))" with
    | (FParsec.CharParsers.ParserResult.Success (value, _, _)) ->
        let result = (ClojureRuntime () :> Runtime<_,_>).eval value
        printfn "%A" result
        Xunit.Assert.Equal(Value.Number 2, result)
    | error -> failwithf "%A" error
// type ReferenceImplementation() =
//     interface Runtime<Value.Token, obj> with
//         member this.compile token =
//             ()
//         member this.eval value =
//             match value with
//             | _ -> value
//         member this.readString(var0) = failwith "todo"