module Clojure.Eval

open Clojure.Read
open Microsoft.FSharp.Reflection

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
    let resolveSymbol (name: string) : Value option =
        locals
        |> List.tryPick (fun items -> items |> Map.tryFind name)
        |> Option.orElseWith (fun () -> ns.TryFind name)
        
    let rec eval item : CompiledValue =
        match item with
        // todo: compile item => evalCompiled
        | Value.TokenList tokens when tokens.Length > 0 ->
            let rec callExpr expr args =
                match expr with
                | Value.Symbol name ->
                    match resolveSymbol name with
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
                | Value.CompiledFn fn ->
                    fn (Array.skip 1 tokens)
                | Value.TokenList values ->
                    callExpr (eval (Value.TokenList values)) args 
                | value ->
                    failwithf "Token %A is not callable" tokens[0]
            callExpr tokens[0] (Array.skip 1 tokens)
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
            match eval values[0] with
            | Value.Boolean true -> eval values[1]
            | _ -> eval values[2]
        ))
        let parseDefnBindings (argNames: Value[]) (values: Value[]) =
            match argNames |> Array.tryFindIndex (function Value.Symbol "&" -> true | _ -> false) with
            | Some i ->
                let args = Array.append (Array.take i argNames) (Array.skip (i + 1) argNames) |> Array.map (function Value.Symbol name -> name)
                let values = Array.append (Array.take i values) [| (Value.Vector (Array.skip (i + 1) values)) |]
                Map.ofArray (Array.zip args values)
            | None -> Map.ofArray (Array.zip (argNames |> Array.map (function Value.Symbol name -> name)) values)
        ns <- ns.Add("fn", Value.Builtin (fun values ->
            // todo: name
            let (Value.Vector args) = values[0]
            Value.CompiledFn (fun fnValues ->
                let argsMap = parseDefnBindings args fnValues
                let mutable result = Value.Null
                locals <- argsMap :: locals
                for expr in (Array.skip 1 values) do // todo
                    result <- eval expr
                locals <- List.tail locals
                result
            )
        ))
        ns <- ns.Add("defn", Value.MacroDefn (fun values ->
            (Value.TokenList [| Value.Symbol "def"; values[0]; Value.TokenList [| Value.Symbol "fn"; yield! (Array.skip 1 values) |] |])
        ))
        ns <- ns.Add("atom", Value.CompiledFn (fun values -> Value.Atom (ref values[0])))
        ns <- ns.Add("deref", Value.CompiledFn (fun values ->
            match values with
            | [| Value.Atom ref |] -> ref.contents
            | _ -> failwithf "Couldn't deref %A" values
        ))
        ns <- ns.Add("reset!", Value.CompiledFn (fun values ->
            match values with
            | [| Value.Atom ref; value |] -> ref.contents <- value
            | _ -> failwithf "Couldn't set %A" values
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
            System.Console.WriteLine(String.concat " " (Array.map string values))
            Value.Null
        ))
        ns <- ns.Add("def", Value.Builtin (fun values ->
            System.Console.WriteLine (sprintf "%A" values)
            match values with
            | [| Value.Symbol name; value |] -> ns <- ns.Add(name, eval value)
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
        ns <- ns.Add("=", CompiledFn (fun (values: Value[]) ->
            match values[0], values[1] with
            | Value.Number n, Value.Number n2 -> n = n2
            |> Value.Boolean
        ))
        ns <- ns.Add("do", Builtin (fun (values: Value[]) ->
            let mutable value = Value.Null
            for expr in values do
                value <- eval expr
            value))
        ns <- ns.Add("loop", Builtin (fun (values: Value[]) ->
            let bindingsMap = parseLetBindings values
            let paramNames = bindingsMap.Keys |> Seq.map Value.Symbol |> Array.ofSeq
            let exprs = Array.tail values
            let rec loop (values: Value[]) =
                let bindingsMap = parseDefnBindings paramNames values
                locals <- bindingsMap :: locals
                let mutable result = Value.Null
                for expr in exprs do
                    result <- eval expr
                locals <- List.tail locals
                result
            let items = bindingsMap.Add("recur", CompiledFn (fun (values: Value[]) ->
                loop values
            ))
            locals <- items :: locals
            let result = loop (Array.ofSeq bindingsMap.Values)
            locals <- List.tail locals
            result
        )) 
    do
        ns <- ns.Add("-", CompiledFn clojure.core.Math.subtract)
        ns <- ns.Add("/", CompiledFn clojure.core.Math.division)
        ns <- ns.Add("*", CompiledFn clojure.core.Math.mult)
        ns <- ns.Add(".", MacroDefn (fun (exprs: Value[]) ->
            let rec unwrap (value: Value) =
                match value with
                | Null -> null
                | _ -> (snd (FSharpValue.GetUnionFields (value, value.GetType())))[0]
            match eval exprs[0], Array.tail exprs with
            | Value.Obj o, [| Value.Symbol s |] -> Value.Obj <| o.GetType().GetMember(s).[0]
            | Value.Obj o, values ->
                match values[0] with
                | Value.Symbol s ->
                    let info = (o.GetType().GetMember(s)[0] :?> System.Reflection.MethodInfo)
                    Value.Obj (info.Invoke(o, Array.map unwrap (Array.map eval <| Array.tail values)))
        ))
    member this.Eval = eval
    member this.UpdateNamespace(name, value) =
        ns <- ns.Add(name, value)
    interface Runtime<Value, CompiledValue> with
        member this.compile token = token
        member this.eval item = eval item
        member this.readString text = FParsec.CharParsers.run Parser.expr text |> function FParsec.CharParsers.ParserResult.Success (token,_,_) -> token

    
[<Xunit.Fact>]
let infix_macro_and_addition () =
    match FParsec.CharParsers.run Parser.value "(infix (1 + 1))" with
    | (FParsec.CharParsers.ParserResult.Success (value, _, _)) ->
        let result = (ClojureRuntime () :> Runtime<_,_>).eval value
        printfn "%A" result
        Xunit.Assert.Equal(Value.Number 2, result)
    | error -> failwithf "%A" error