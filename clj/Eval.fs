module Clojure.Eval

open System
open System.Collections
// open System.Collections.Immutable
open System.Reflection
open Clojure.Read
// open FSharpx.Option
open Microsoft.FSharp.Reflection
type System.String with
    member this.Split(s: string) =
        this.Split([| s |], StringSplitOptions.RemoveEmptyEntries)

let trace o =
    Console.ForegroundColor <- ConsoleColor.Blue
    printfn "%A" o
    
    Console.ResetColor()

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
    | _ -> (snd (FSharpValue.GetUnionFields (value, value.GetType()))).[0]
    
type ClojureRuntime () as this =
    let mutable locals: Scope list = []
    // let mutable currentNs = "user"
    let mutable ns = Map.empty
    let mutable assemblyCache =
        AppDomain.CurrentDomain.GetAssemblies()
        |> Array.map (fun a -> a.GetName().Name, a)
        |> fun a -> Array.iter (printfn "%A") a; a
        |> Map.ofArray
        
    let mutable types =
        AppDomain.CurrentDomain.GetAssemblies()
        |> Array.map (fun a -> try Some (a.GetName().Name, a.GetTypes()) with _ -> None)
        |> Array.choose id
        |> Array.map snd
        |> Array.reduce Array.append
        |> Array.map (fun t -> t.Namespace + "." + t.Name, t)
        |> Map.ofArray
    let mutable ns = ref { name = "user"; values = Map.empty; imports = Map.empty; types = Map.empty }
    // let mutable nsRef = ref ns
    let mutable namespaces: Map<Name, Namespace ref> = Map.empty.Add(ns.contents.name, ns)
    let __def (name, value) = 
        ns.contents <- { ns.contents with values = ns.contents.values.Add(name, value) }
    let resolveSymbol (name: string) : Value option =
        // trace $"resolveSymbol {name}"
        locals
        |> List.tryPick (fun items -> items |> Map.tryFind name)
        |> Option.orElseWith (fun () -> this.Ns.values.TryFind name)
        |> Option.orElseWith (fun () ->
            if name.Contains "/" then
                let parts = name.Split "/"
                namespaces.TryFind parts.[0]
                |> Option.bind(fun scope -> scope.Value.values.TryFind parts.[1])
                |> Option.orElseWith (fun () ->
                    trace "trying or else with"
                    let name = parts.[0]
                    let _memberName = parts.[1]
                    try
                        let t = types[name]
                            // let _t = Type.GetType(name)
                            // // if _t = null then Type.GetType($"{name}, {name}") else _t
                            // if _t = null then Type.GetType(sprintf "%s, %s" name name) else _t
                        let _member = t.GetMethods() |> Array.filter (fun m -> m.Name = parts[1])
                        Some (Obj _member)
                    with error ->
                        printfn "%A" error
                        None)
                        //Some Obj _member with error -> None)
            else None)
    let changeNs name =
        if not (namespaces.ContainsKey name) then
            namespaces <- namespaces.Add (name, ref namespaces.["user"].contents)
        // currentNs <- name
        ns <- namespaces.[name]
    let mutable callstack = []
    let mutable unwinding = false
    let rec wrapValue (value: obj) =
        match value with
        | :? string as s -> String s
        | :? int as n -> Number (int64 n)
        | :? int64 as n -> Number n
        | :? double as n -> Float n
        | :? single as n -> Float (float n)
        | :? bool as value -> Boolean value
        | null -> Null
        | _ -> Obj value
    let rec eval item : CompiledValue =
        // todo: Evaluation frame (as an alternative to per-thread frame)
        let rec loopEval (*stack*) item =
            let eval = ()
            let rt = this
            match item with
            | Value.List tokens when tokens.Length > 0 ->
                let rec callExpr expr (args: Value[]) =
                    match expr with
                    | Value.Obj o ->
                        match o with
                        | :? MethodInfo as info ->
                            // let resolvedArgs = args |> Array.map (loopEval (info.Name :: stack)) |> Array.map unwrap
                            let resolvedArgs = args |> Array.map loopEval |> Array.map unwrap
                            wrapValue <| info.Invoke(null, resolvedArgs)
                        | :? (MethodInfo[]) as info ->
                            let resolvedArgs = args |> Array.map loopEval |> Array.map unwrap
                            let argsMatch (info: MethodInfo) =
                                let paramInfos = info.GetParameters()
                                if not (info.Name.StartsWith "get_" && info.IsSpecialName) && paramInfos.Length = resolvedArgs.Length then
                                    paramInfos
                                    |> Array.zip resolvedArgs
                                    |> Array.forall (fun (arg, p) ->
                                        arg.GetType() = p.ParameterType)
                                else false
                            let methodInfo = info |> Array.find argsMatch
                            wrapValue <| methodInfo.Invoke(null, resolvedArgs)
                        // | :? (MemberInfo[]) as infos ->
                        //     let resolvedArgs = args |> Array.map (loopEval ("MemberInfo[] Eval" :: stack))
                        //     printfn "args = %A" args
                        //     printfn "resolvedArgs = %A" resolvedArgs
                        //     printfn "info %A" infos
                        //     Null
                        | _ ->
                            failwithf "Object %A is not callable" o
                    | Value.Symbol name ->
                        // printfn $"Calling {name}"
                        match resolveSymbol name with
                        | Some (Value.MacroDefn fn) ->
                            // loopEval (name :: stack) (fn (Array.tail tokens))
                            loopEval (fn (Array.tail tokens))
                        | Some (Value.CompiledFn fn) ->
                            try
                                let resolvedArgs =
                                    args |> Array.map loopEval // (loopEval (name :: stack))
                                callstack <- (name, resolvedArgs) :: callstack
                                let result = fn resolvedArgs
                                callstack <- List.tail callstack
                                result
                            with error ->
                                if not unwinding then
                                    printfn "Call stack = %A" callstack
                                    unwinding <- true
                                if callstack.Length = 0 then
                                    unwinding <- false
                                raise error
                            // with error ->
                            //     printfn "%A" error
                            //     printfn "%A" callstack
                            //     callstack <- List.tail callstack
                            //     raise error
                        | Some (Value.ExprEvaluator form) ->
                            form (Array.skip 1 tokens)
                        | Some value ->
                            // | _ ->
                                callExpr value args
                    | Value.CompiledFn fn ->
                        fn (Array.skip 1 tokens)
                    | Value.List values ->
                        // callExpr (loopEval stack (Value.List values)) args
                        callExpr (loopEval (Value.List values)) args
                    | Value.Keyword keyword ->
                        match loopEval args.[0] with
                        // | Value.HashSet values -> Value.Null
                        | Value.Map values -> loopEval values.[string (Keyword keyword)]
                        | _else -> Value.Null
                    | value ->
                        failwithf "Token %A is not callable" tokens.[0]
                callExpr tokens.[0] (Array.skip 1 tokens)
            | Value.InlineFunc values ->
                CompiledFn <| fun (args: Value[]) ->
                    let _locals = locals
                    let vars = Map.ofArray (args |> Array.mapi (fun index arg -> "%" + string index, arg))
                    locals <- vars.Add("%", vars.["%0"]) :: locals
                    
                    let result = loopEval (List values)
                    locals <- _locals
                    result
            | Value.Symbol name ->
                resolveSymbol name |> Option.get |> loopEval
            // | Value.Obj value ->
            //     match value with
            //     | :? MethodInfo as info ->
            //         info.Invoke(null, [||])
            //     | _ -
            // | Value.Metadata (metadata, value) -> value
            | item -> item
        loopEval item
    
    let evalString (s: string) =
        FParsec.CharParsers.run Parser.file s |> (function | FParsec.CharParsers.Success (result, _, _) -> result |> List.map (fst >> snd >> eval)) // (snd (fst result)))
    let parseLetBindingsIntoArray (exprs: Value[]) =
        let (Value.Vector bindings) = exprs.[0]
        let chunks = bindings |> Array.chunkBySize 2 |> Array.map (fun a -> a.[0], a.[1])
        let _locals = locals
        let items = [|
            for (Symbol name, value) in chunks do
                let result = (name, eval value)
                locals <- Map.ofList [result] :: locals
                yield result
        |]
        locals <- _locals
        items
    let parseLetBindings (exprs: Value[]) =
        // let (Value.Vector bindings) = exprs[0]
        // let chunks = bindings |> Array.chunkBySize 2 |> Array.map (fun a -> a[0], a[1])
        // let _locals = locals
        // let items = [
        //     for (Symbol name, value) in chunks do
        //         let result = (name, eval value)
        //         locals <- Map.ofList [result] :: locals
        //         yield result
        // ]
        // locals <- _locals
        // let names = bindings |> Array.chunkBySize 2 |> Array.map Array.head |> Array.map (fun (Value.Symbol name) -> name)
        // let values = bindings |> Array.chunkBySize 2 |> Array.map (fun a -> a[1]) |> Array.map eval
        // Map.ofArray (Array.zip names values)
        // Map.ofList items
        Map.ofArray (parseLetBindingsIntoArray exprs)
    let parseMacroBindings (exprs: Value[]) =
        let (Value.Vector bindings) = exprs.[0]
        let names = bindings |> Array.chunkBySize 2 |> Array.map Array.head |> Array.map (fun (Value.Symbol name) -> name)
        let values = bindings |> Array.chunkBySize 2 |> Array.map (fun a -> a.[1])
        Map.ofArray (Array.zip names values)
    let _map = fun (values: Value[]) ->
         let (CompiledFn fn) = values.[0]
         let (Sequence iter) = values.[1]
         
         Sequence (seq {
             for var in iter do
                 fn [| var |]
         })
        
    let _forloop = fun (values: Value[]) ->
         let (Vector letBindings) = values.[0]
         let (Symbol varName) = letBindings.[0]
         let (Sequence iter) = eval letBindings.[1]
         
         let _locals = locals
         
         let result = seq {
             for var in iter do
                 locals <- Map.empty.Add(varName, var) :: _locals
                 let mutable result = eval values.[1]
                 for expr in Array.skip 2 values do
                     result <- eval expr
                 yield result
         }
         
         Sequence result
    do
        __def("ns", Value.ExprEvaluator (fun values ->
            match values.[0] with
            | Value.Symbol name -> changeNs name
            | _ -> failwithf "Unexpected type for namespace symbol %A values" values
            Value.Null
        ))
        __def("let", Value.ExprEvaluator (fun values ->
            locals <- parseLetBindings values :: locals
            let mutable result = Value.Null
            for expr in values do
                result <- eval expr
            locals <- List.tail locals
            result
        ))
        __def("if", Value.ExprEvaluator (fun values ->
            match eval values.[0] with
            | Value.Boolean true -> eval values.[1]
            | _ -> eval values.[2]
        ))
        let parseDefnBindings (argNames: Value[]) (values: Value[]) =
            match argNames |> Array.tryFindIndex (function Value.Symbol "&" -> true | _ -> false) with
            | Some i ->
                let args = Array.append (Array.take i argNames) (Array.skip (i + 1) argNames) |> Array.map (function Value.Symbol name -> name)
                let values = Array.append (Array.take i values) [| (Value.Vector (Array.skip (i + 1) values)) |]
                Map.ofArray (Array.zip args values)
            | None -> Map.ofArray (Array.zip (argNames |> Array.map (function Value.Symbol name -> name)) values)
        __def("fn", Value.ExprEvaluator (fun (values: Value[]) ->
            let (Value.Vector args) = values.[0]
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
        __def("defn", Value.MacroDefn (fun (values: Value[]) ->
            (Value.List [| Value.Symbol "def"; values.[0]; Value.List [| Value.Symbol "fn"; yield! (Array.skip 1 values) |] |])
        ))
        __def("eval", Value.MacroDefn (fun (values: Value[]) ->
            match values with
            | [| Quote expr |] -> eval expr | _ -> eval values.[0]))
        __def("cond", Value.MacroDefn <| fun (values: Value[]) ->
            let b = values |> Array.chunkBySize 2 |> Array.map (fun a -> a.[0], a.[1])
            let rec loop exprs =
                match exprs with
                | [| case; expr |] ->
                    if string case = string (Keyword "else") then
                        expr
                    else
                        List [| Symbol "if"; case; expr; Null |]
                | _ ->
                    List [| Symbol "if"; exprs.[0]; exprs.[1]; loop <| Array.skip 2 exprs |]
            loop values
        )            
        __def("atom", Value.CompiledFn (fun values -> Value.Atom (Atom.Init<_> (values.[0]))))
        __def("deref", Value.CompiledFn (fun values ->
            match values with
            | [| Value.Atom ref |] -> Atom.get<_> ref
            | _ -> failwithf "Couldn't deref %A" values
        ))
        __def("reset!", Value.CompiledFn (fun values ->
            match values with
            | [| Value.Atom ref; value |] -> Atom.set ref value
            | _ -> failwithf "Couldn't call reset! with values %A" values
            Value.Null
        ))
        __def("swap!", Value.CompiledFn (fun values ->
            match values with
            | [| Value.Atom ref; Value.CompiledFn fn |] ->
                ref.swap (fun value -> fn [| value |])
            | _ -> failwithf "Couldn't call swap! with values %A" values
            Value.Null
        ))
        __def("+", Value.CompiledFn (fun values ->
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
        __def("infix", Value.MacroDefn <| (fun tokens ->
            match tokens.[0] with
            | Value.List tokens ->
                Value.List [| tokens.[1]; tokens.[0]; yield! (Array.skip 2 tokens) |]
            | _ -> failwithf "Invalid args for infix: %A" tokens
        ))
        __def("println", Value.CompiledFn (fun values ->
            System.Console.WriteLine(String.concat " " (Array.map string values))
            Value.Null
        ))
        __def("print", CompiledFn <| fun values ->
            Console.Write(String.concat " " (Array.map string values))
            Value.Null)
        __def("def", Value.ExprEvaluator (fun values ->
            // System.Console.WriteLine (sprintf "%A" values)
            match values with
                // __def(name, eval value)
            // | [| Value.Metadata (_, Value.Symbol name); value |]
            | [| Value.Symbol name; value |] ->
                this.UpdateNamespace(name, eval value)
            Value.Null
        ))
        
        __def("defmacro", Value.MacroDefn <| (fun (tokens: Value[]) ->
            match tokens.[0] with
            | Value.Symbol name ->
                __def(name, Value.MacroDefn <| (fun items ->
                    let (Value.Vector macroArgumentNames) = tokens.[1]
                    let macroBindings = Map.ofArray <| Array.zip (Array.map (function Value.Symbol name -> name) macroArgumentNames) items
                    // locals <- (macroBindings |> Map.map (fun name value -> value)) :: locals
                    locals <- macroBindings :: locals
                    for i in 2..(tokens.Length - 2) do
                        eval tokens.[i]
                        |> ignore
                    let rec loop expr =
                        match expr with
                        | Value.Quote item -> loop item
                        | Value.QuoteSyntax item -> loop item
                        | Value.List items ->
                            // todo: Handle ~@
                            Value.List (Array.map loop items)
                        | Value.Unquote (Value.Symbol item) ->
                            macroBindings.[item]
                        | Value.Unquote item -> eval item
                            // match eval item with
                            // | Value value -> value
                            // | value -> failwithf "Unexpected evaluation term result in macro expasion:\n%A" value
                        | item -> item
                        | _ -> failwithf "Unexpected token: %A" expr
                    let result = loop tokens.[tokens.Length - 1]
                    locals <- List.tail locals
                    result
                ))
            | value -> failwithf "Error: given %A for symbol name" value
            Value.Null
        ))
        __def("=", CompiledFn (fun (values: Value[]) ->
            match values.[0], values.[1] with
            | Value.Number n, Value.Number n2 -> n = n2
            | Value.Float f, Value.Float f2 -> f = f2
            | _, Null | Null, _ -> false
            | String s, String s2 -> s = s2
            | Boolean b, Boolean c -> b = c
            |> Value.Boolean
        ))
        __def("do", ExprEvaluator (fun (values: Value[]) ->
            let mutable value = Value.Null
            for expr in values do
                value <- eval expr
            value))
        __def("when", MacroDefn <| fun (values: Value[]) ->
            // todo:
            // (defmacro when [condition & values]
            //     `(if ~condition (do values)))
            // (defmacro asdf [condition & values] `(if ~condition (do ~@values)))
            List [|
                Symbol "if"
                values.[0]
                List [|
                    Symbol "do"
                    yield! (Array.tail values)
                |]
                Null
            |])
        __def("loop", ExprEvaluator (fun (values: Value[]) ->
            let bindings = parseLetBindingsIntoArray values
            let bindingsMap = Map.ofArray bindings
            // let paramNames = bindingsMap.Keys |> Seq.map Value.Symbol |> Array.ofSeq
            let paramNames = bindingsMap |> Seq.map (fun kv -> kv.Key) |> Seq.map Value.Symbol |> Array.ofSeq
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
            let loopArgs = bindings |> Array.map snd
            let result = loop loopArgs
            locals <- List.tail locals
            result
        ))
        
    let _try = ExprEvaluator (fun values ->
        Null)
    do
        __def("-", CompiledFn clojure.core.Math.subtract)
        __def("/", CompiledFn clojure.core.Math.division)
        __def("*", CompiledFn clojure.core.Math.mult)
        __def(".", MacroDefn <| (fun (exprs: Value[]) ->
            match eval exprs.[0], Array.tail exprs with
            | Value.Obj o, [| Value.Symbol s |] -> Value.Obj << box <| o.GetType().GetMember(s).[0]
            | Value.Obj o, values ->
                match values.[0] with
                | Value.Symbol s ->
                    let m = o.GetType().GetMember(s).[0]
                    let info = (m :?> System.Reflection.MethodInfo)
                    Value.Obj (info.Invoke(o, Array.map unwrap (Array.map eval <| Array.tail values)))
            | n, [| Value.Symbol s |] ->
                match (unwrap n).GetType().GetMember(s).[0] with
                | :? MethodInfo as info -> wrapValue <| info.Invoke(unwrap n, [||])
                | _ -> Value.Obj << box <| (unwrap n).GetType().GetMember(s).[0]
        ))
        __def ("defrecord", ExprEvaluator <| fun values ->
            let (Value.Symbol name) = values.[0]
            let (Value.Vector fields) = values.[1]
            let structFields = fields |> Array.map (function
                // | Value.Metadata (Value.Symbol fieldType, Value.Symbol fieldName) -> fieldType, fieldName
                | Value.Symbol fieldName -> "dynamic", fieldName
            )
            this.DefRecord (name, structFields)
            Value.Null)
        __def ("deftype", ExprEvaluator <| fun values ->
            let (Value.Symbol name) = values.[0]
            let (Value.Vector fields) = values.[1]
            let structFields = fields |> Array.map (fun value ->
                // | Value.Metadata (Value.Symbol fieldType, Value.Symbol fieldName) -> fieldType, fieldName
                match value with
                | Value.Symbol fieldName ->
                    match varDataTable.TryGetValue value with
                    | true, Symbol typeName ->
                        // printfn "%A metadata:\n\t%A" value metadata
                        typeName, fieldName
                    | _ ->
                        "dynamic", fieldName
            )
            
            // let constructor = fun (values: Value[]) ->
            let args =
                String.concat " " (structFields
                                   |> Array.map (fun (t, name) -> sprintf "^%s %s" t name))// $"^{t} {name}"))
            let map =
                String.concat " " (structFields
                                   |> Array.map (fun (t, name) -> sprintf ":%s %s" name name)) // $":{name} name"))
            let result =
                evalString (sprintf "
                (defn ->{%s} [{%s}]
                  {{{%s}}}
                )
                (defn {%s}. [{%s}]
                  {{{%s}}}
                )
                " name args map name args map)
                // evalString $"
                // (defn ->{name} [{args}]
                //   {{{map}}}
                // )
                // (defn {name}. [{args}]
                //   {{{map}}}
                // )
                // "
                // eval (List [|
                //     Symbol "defn"; Vector [| for (typeName, fieldName) in structFields do yield String fieldName |]
                //     let items = Generic.Dictionary()
                //     let mutable index = 0
                //     for (_, name) in structFields do
                //         items.Add(Keyword name, )
                //     Value.Map items
                // |])
            
            // _def("->" + name, CompiledFn, constructor)    
            this.DefType (name, structFields)
            Value.Null)
        this.MultipleDefs [|
            "defrecord", ExprEvaluator <| fun (values: Value[]) ->
                let (Value.Symbol name) = values.[0]
                let (Value.Vector fields) = values.[1]
                let structFields = fields |> Array.map (function
                    // | Value.Metadata (Value.Symbol fieldType, Value.Symbol fieldName) -> fieldType, fieldName
                    | Value.Symbol fieldName -> "dynamic", fieldName
                )
                this.DefRecord (name, structFields)
                Value.Null
            "debug", CompiledFn <| fun values ->
                if values.Length > 0 then values.[0] else Null
            "type", CompiledFn <| fun (values: Value[]) ->
                Value.Obj << box <| (unwrap values.[0]).GetType()
            "<", CompiledFn (clojure.core.Math.lt)
            ">", CompiledFn (clojure.core.Math.gt)
            "mod", CompiledFn (clojure.core.Math._mod)
            "int", CompiledFn ((fun (values: Value[]) ->
                match values.[0] with
                | Number n -> values.[0]
                | Float f -> Number <| int64 f
                ))
            "Math/sqrt", CompiledFn <| fun (values: Value[]) ->
                match values.[0] with
                | Number n -> System.Math.Sqrt(float n) |> Float
                | Float n -> System.Math.Sqrt(n) |> Float
            "Math/abs", clojure.core.Math.abs
                
            "assoc", CompiledFn (fun (values: Value[]) ->
                match values.[0] with
                | Map m ->
                    // m[values[1]] <- values[2]
                    let m = if m.ContainsKey (string values.[1]) then m.Remove(string values.[1]) else m
                    Map <| m.Add(string values.[1], values.[2])
                | _ -> Value.Null
            )
            
            "dissoc", CompiledFn <| (fun (values: Value[]) ->
                match values.[0] with
                | Map m ->
                    // m[values[1]] <- values[2]
                    // if m.ContainsKey(values[1]) then
                    //     m.Remove(values[1]) |> ignore
                    Map <| m.Remove(string values.[1])
                | _ -> Value.Null
            )
            
            "hash-map", CompiledFn <| fun (values: Value[]) ->
                // let mutable d = ImmutableDictionary.Empty
                let mutable d = Map.empty
                for kv in (values |> Array.chunkBySize 2) do
                    d <- d.Add(string kv.[0], kv.[1])
                    // d[kv[0]] <- kv[1]
                Map d
                
            "for",  ExprEvaluator _forloop
            "pmap", CompiledFn _map
            "doseq", MacroDefn <| fun (values: Value[]) ->
                let (Sequence iter) = eval (List [| Symbol "for"; values.[0]; yield! Array.tail values |])
                List [|
                    CompiledFn <| fun (values: Value[]) -> iter |> Seq.iter (id >> ignore); Null
                |]
            "range", CompiledFn <| fun (values: Value[]) ->
                let (Number n) = values.[0]
                Sequence <| seq { for i in 0L..(n - 1L) do yield Number i }
            "read-line", CompiledFn (fun _ ->
                String (Console.ReadLine()))
            "types", CompiledFn (fun _ ->
                // Map <| ImmutableDictionary<_,_>(types |> Map.map (fun id value -> Obj value)))
                Map <| (types |> Map.map (fun id value -> Obj value)))
            "dir", CompiledFn (fun _ -> Obj this.Ns.values)
            // ``try`` = _try
        |]
    member this.Cache = assemblyCache
    member this.Types = types
    member this.Ns = ns.contents
    member this.Eval value = eval value
    member this.MultipleDefs(defs: (string * Value)[]) =
        for item in defs do
            this.UpdateNamespace(fst item, snd item)
    member this.MultipleDefs(value: obj) =
        for item in value.GetType().GetProperties() do
            if item.PropertyType = typeof<Value> then
                this.UpdateNamespace(item.Name, (item.GetValue value) :?> Value)
            if item.PropertyType = typeof<Value[] -> Value> then
                this.UpdateNamespace(item.Name, CompiledFn <| (item.GetValue value :?> Value[] -> Value))
    // member _def (name: string, constructor: obj -> Value, fn: Value[] -> Value) =
    //     ns.contents <- { ns.contents with values = ns.contents.values.Add(name, constructor (box fn)) }
    // member this._def (name: string, value) =
    //     ns.contents <- { ns.contents with values = ns.contents.values.Add(name, value) }
        // nsRef.contents <- ns
    member this.DefType(name: string, fields: (string * string)[]) =
        ns.contents <- { ns.contents with types = ns.contents.types.Add(name, Class (name, fields)) }
        printfn "%A" ns.contents
    member this.Eval (s: string) = evalString s
    member this.DefRecord(name: string, fields: (string * string)[]) =
        ns.contents <- { ns.contents with types = ns.contents.types.Add(name, Record (name, fields)) }
        let args =
            String.concat " "
                (fields
                 |> Array.map (fun (t, name) -> sprintf "^%s %s" t name))
        let map =
            String.concat " "
                (fields
                 // |> Array.map (fun (t, name) -> $":{name} {name}"))
                 |> Array.map (fun (t, name) -> sprintf ":%s %s" name name))
        let result =
            evalString (sprintf "
            (defn ->{%s} [{%s}]
              {{{%s}}}
            )
            (defn {%s}. [{%s}]
              {{{%s}}}
            )
            " name args map name args map)
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