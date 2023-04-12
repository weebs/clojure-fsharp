module Clojure.Eval

open Clojure.Read
// open FSharpx.Option
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
type Namespace = { name: string; values: Scope; imports: Scope; types: Map<string, ClojureType> }
let rec unwrap (value: Value) =
    match value with
    | Null -> null
    | _ -> (snd (FSharpValue.GetUnionFields (value, value.GetType())))[0]
type ClojureRuntime () as this =
    let mutable locals: Scope list = []
    // let mutable currentNs = "user"
    let mutable ns = Map.empty
    let mutable ns = ref { name = "user"; values = Map.empty; imports = Map.empty; types = Map.empty }
    // let mutable nsRef = ref ns
    let mutable namespaces: Map<Name, Namespace ref> = Map.empty.Add(ns.contents.name, ns)
    let resolveSymbol (name: string) : Value option =
        locals
        |> List.tryPick (fun items -> items |> Map.tryFind name)
        |> Option.orElseWith (fun () -> this.Ns.values.TryFind name)
        |> Option.orElseWith (fun () ->
            if name.Contains "/" then
                let parts = name.Split "/"
                namespaces.TryFind parts[0]
                |> Option.bind(fun scope -> scope.Value.values.TryFind parts[1])
            else None)
    let changeNs name =
        if not (namespaces.ContainsKey name) then
            namespaces <- namespaces.Add (name, ref namespaces.["user"].contents)
        // currentNs <- name
        ns <- namespaces[name]
        
    let rec eval item : CompiledValue =
        let rec innerLoop stack item =
            let eval = ()
            match item with
            // todo: compile item => evalCompiled
            | Value.TokenList tokens when tokens.Length > 0 ->
                let rec callExpr expr (args: Value[]) =
                    match expr with
                    | Value.Symbol name ->
                        match resolveSymbol name with
                        | Some (Value.MacroDefn fn) ->
                            innerLoop (name :: stack) ((fn :?> _) (Array.tail tokens))
                        | Some (Value.CompiledFn fn) ->
                            // todo: resolve Symbols before call
                            (fn :?> _) (Array.tail tokens |> Array.map (innerLoop (name :: stack)))
                        | Some (Value.ClojureForm form) ->
                            (form :?> _) (Array.skip 1 tokens)
                        | Some value -> value
                        | _ ->
                            failwithf "Token %A is not callable" tokens[0]
                    | Value.CompiledFn fn ->
                        (fn :?> _) (Array.skip 1 tokens)
                    | Value.TokenList values ->
                        callExpr (innerLoop stack (Value.TokenList values)) args
                    // | Value.Keyword keyword ->
                    //     match args[0] with
                    //     | Value.Symbol symbol ->
                    //         match resolveSymbol symbol with
                    //         | Some (Value.HashSet)
                    | Value.Keyword keyword ->
                        match innerLoop stack args.[0] with
                        // | Value.HashSet values -> Value.Null
                        | Value.Map values -> values[keyword]
                        | _else -> Value.Null
                    | value ->
                        failwithf "Token %A is not callable" tokens[0]
                callExpr tokens[0] (Array.skip 1 tokens)
            | Value.Symbol name ->
                resolveSymbol name |> Option.get
                
                // locals
                // |> List.tryPick (fun items -> items |> Map.tryFind name)
                // |> Option.defaultWith (fun () -> ns.contents.values[name])
                // |> function (Value i) -> i
            // | Value.Metadata (metadata, value) -> value
            | item -> item
        innerLoop [] item
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
        this.Def("ns", Value.ClojureForm, (fun values ->
            match values[0] with
            | Value.Symbol name -> changeNs name
            | _ -> failwithf "Unexpected type for namespace symbol %A values" values
            Value.Null
        ))
        this.Def("let", Value.ClojureForm, (fun values ->
            locals <- parseLetBindings values :: locals
            let mutable result = Value.Null
            for expr in values do
                result <- eval expr
            locals <- List.tail locals
            result
        ))
        this.Def("if", Value.ClojureForm, (fun values ->
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
        this.Def("fn", Value.ClojureForm, (fun values ->
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
        this.Def("defn", Value.MacroDefn, (fun values ->
            (Value.TokenList [| Value.Symbol "def"; values[0]; Value.TokenList [| Value.Symbol "fn"; yield! (Array.skip 1 values) |] |])
        ))
        this.Def("atom", Value.CompiledFn, (fun values -> Value.Atom (Atom.Init<_> (values.[0]))))
        this.Def("deref", Value.CompiledFn, (fun values ->
            match values with
            | [| Value.Atom ref |] -> Atom.get<_> ref
            | _ -> failwithf "Couldn't deref %A" values
        ))
        this.Def("reset!", Value.CompiledFn, (fun values ->
            match values with
            | [| Value.Atom ref; value |] -> Atom.set ref value
            | _ -> failwithf "Couldn't call reset! with values %A" values
            Value.Null
        ))
        this.Def("swap!", Value.CompiledFn, (fun values ->
            match values with
            | [| Value.Atom ref; Value.CompiledFn fn |] ->
                ref.swap (fun value -> (fn :?> _) [| value |])
            | _ -> failwithf "Couldn't call swap! with values %A" values
            Value.Null
        ))
        this.Def("+", Value.CompiledFn, (fun values ->
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
        this.Def("infix", Value.MacroDefn, (fun tokens ->
            match tokens[0] with
            | Value.TokenList tokens ->
                Value.TokenList [| tokens[1]; tokens[0]; yield! (Array.skip 2 tokens) |]
            | _ -> failwithf "Invalid args for infix: %A" tokens
        ))
        this.Def("println", Value.CompiledFn, (fun values ->
            System.Console.WriteLine(String.concat " " (Array.map string values))
            Value.Null
        ))
        this.Def("def", Value.ClojureForm, (fun values ->
            System.Console.WriteLine (sprintf "%A" values)
            match values with
                // this.Def(name, eval value)
            // | [| Value.Metadata (_, Value.Symbol name); value |]
            | [| Value.Symbol name; value |] ->
                this.UpdateNamespace(name, eval value)
            Value.Null
        ))
        
        this.Def("defmacro", Value.MacroDefn, (fun tokens ->
            match tokens[0] with
            | Value.Symbol name ->
                this.Def(name, Value.MacroDefn, (fun items ->
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
        this.Def("=", CompiledFn, (fun (values: Value[]) ->
            match values[0], values[1] with
            | Value.Number n, Value.Number n2 -> n = n2
            |> Value.Boolean
        ))
        this.Def("do", ClojureForm, (fun (values: Value[]) ->
            let mutable value = Value.Null
            for expr in values do
                value <- eval expr
            value))
        this.Def("loop", ClojureForm, (fun (values: Value[]) ->
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
        this.Def("-", CompiledFn, clojure.core.Math.subtract)
        this.Def("/", CompiledFn, clojure.core.Math.division)
        this.Def("*", CompiledFn, clojure.core.Math.mult)
        this.Def(".", MacroDefn, (fun (exprs: Value[]) ->
            match eval exprs[0], Array.tail exprs with
            | Value.Obj o, [| Value.Symbol s |] -> Value.Obj <| o.GetType().GetMember(s).[0]
            | Value.Obj o, values ->
                match values[0] with
                | Value.Symbol s ->
                    let info = (o.GetType().GetMember(s)[0] :?> System.Reflection.MethodInfo)
                    Value.Obj (info.Invoke(o, Array.map unwrap (Array.map eval <| Array.tail values)))
        ))
        this.Def ("defrecord", ClojureForm, fun values ->
            let (Value.Symbol name) = values[0]
            let (Value.Vector fields) = values[1]
            let structFields = fields |> Array.map (function
                // | Value.Metadata (Value.Symbol fieldType, Value.Symbol fieldName) -> fieldType, fieldName
                | Value.Symbol fieldName -> "dynamic", fieldName
            )
            this.DefRecord (name, structFields)
            Value.Null)
        this.Def ("deftype", ClojureForm, fun values ->
            let (Value.Symbol name) = values[0]
            let (Value.Vector fields) = values[1]
            let structFields = fields |> Array.map (function
                // | Value.Metadata (Value.Symbol fieldType, Value.Symbol fieldName) -> fieldType, fieldName
                | Value.Symbol fieldName -> "dynamic", fieldName
            )
            this.DefType (name, structFields)
            Value.Null)
        this.MultipleDefs {|
            defrecord = ClojureForm << box <| fun (values: Value[]) ->
                let (Value.Symbol name) = values[0]
                let (Value.Vector fields) = values[1]
                let structFields = fields |> Array.map (function
                    // | Value.Metadata (Value.Symbol fieldType, Value.Symbol fieldName) -> fieldType, fieldName
                    | Value.Symbol fieldName -> "dynamic", fieldName
                )
                this.DefRecord (name, structFields)
                Value.Null
            ``type`` = CompiledFn << box <| fun (values: Value[]) ->
                Value.Obj <| (unwrap values[0]).GetType()
        |}
    member this.Ns = ns.contents
    member this.Eval = eval
    member this.MultipleDefs(value: obj) =
        for item in value.GetType().GetProperties() do
            if item.PropertyType = typeof<Value> then
                this.UpdateNamespace(item.Name, (item.GetValue value) :?> Value)
    member this.Def (name: string, constructor, fn: Value[] -> Value) =
        ns.contents <- { ns.contents with values = ns.contents.values.Add(name, constructor (box fn)) }
        // nsRef.contents <- ns
    member this.DefType(name: string, fields: (string * string)[]) =
        ns.contents <- { ns.contents with types = ns.contents.types.Add(name, Class (name, fields)) }
        printfn "%A" ns.contents
    member this.DefRecord(name: string, fields: (string * string)[]) =
        ns.contents <- { ns.contents with types = ns.contents.types.Add(name, Record (name, fields)) }
        printfn "%A" ns.contents
    member this.Namespaces = namespaces |> Map.map (fun name value -> value.contents)
    member this.UpdateNamespace(name, value) =
        ns.contents <- { ns.contents with values = ns.contents.values.Add(name, value) }
    interface Runtime<Value, CompiledValue> with
        member this.compile token = token
        member this.eval item = eval item
        member this.readString text = FParsec.CharParsers.run Parser.expr text |> function FParsec.CharParsers.ParserResult.Success (token,_,_) -> token

    
// [<Xunit.Fact>]
// let infix_macro_and_addition () =
//     match FParsec.CharParsers.run Parser.value "(infix (1 + 1))" with
//     | (FParsec.CharParsers.ParserResult.Success (value, _, _)) ->
//         let result = (ClojureRuntime () :> Runtime<_,_>).eval value
//         printfn "%A" result
//         Xunit.Assert.Equal(Value.Number 2, result)
//     | error -> failwithf "%A" error