module Clojure.ReadEval

open System.Reflection.PortableExecutable
open Clojure.Parser
// open System
// open System.Text
// open FParsec
open FSharp.Data.LiteralProviders
open FSharp.Reflection

module Runtime =
    type FunctionSignature = { ident: string; args: (string * string option) list; returns: string option }
    type Value =
        | Int of int64
        | Float of double
        | String of string
        | Null
        | Fn of string[] * Token[]
        | CompiledFn of (Value[] -> Value)
        | MacroDefn of (Expr -> Value)
        | Quote of Token
        | Runtime of obj
        with
        static member toString (value: Value) =
            match value with
            | String s -> s
            | Int i -> i.ToString()
            | Float d -> d.ToString()
            | Null -> "null"
            | Fn (args, defn) -> "Fn[]"
            | Quote tokens -> sprintf "Quote %A" tokens
            | Runtime obj -> if obj <> null then obj.ToString() else "Runtime(null)"
    and Expr =
        // | Function of FunctionSignature
        | Def of ident: string * value: Value
        | ValueLiteral of Value
        | Ident of string
        | Vector of Expr[]
        | Call of callee: Expr * args: Expr[]
        | Unmatched of obj
        // | Runtime of obj
    let printExpr expr =
        match expr with
        | ValueLiteral o -> o.ToString() | expr -> expr.ToString()
    type Function = FunctionSignature * Token[]
    type Namespace = { bound: Map<string, Value> }
    let mutable ns = { bound = Map.ofList [] }
    let mutable evalRef = Unchecked.defaultof<_>
    // do ns <- { ns with bound = ns.bound.Add("print", box (fun (args: Token[]) -> printfn "print" ; for arg in args do match arg with | Token.Item ident -> printf $"{ns.bound[ident]}" | arg -> printf $"{evalRef arg}")) }
    // do ns <- { ns with bound = ns.bound.Add("println", box (fun (args: Token[]) -> (ns.bound["print"] :?> (Token[] -> obj)) args |> ignore; printfn "lol"; null)) }

    let print_macro_Version (args: Expr[]) =
        ()
    // let print (args: obj[]) =
    //     printf "%s" (String.concat " " (args |> Array.map (fun a -> if a = null then "null" else a.ToString())))
    let print (args: Value[]) =
        printf "clojure-fsharp>%s" (String.concat " " (args |> Array.map Value.toString))
    // let print (args: obj[]) =
    //     printf "%s" (String.concat " " (args |> Array.map (fun a -> if a = null then "null" else a.ToString())))
        // printf "%s" (args |> Array.map (function | Ident name -> ns.bound[name] :?> Expr | expr -> expr) |> Array.map (printExpr) |> String.concat " ")
    let inline addBinding name binding = ns <- { ns with bound = ns.bound.Add(name, binding) }
    do addBinding "print" <| Runtime print
    // do addBinding "println" <| Runtime (fun (args: Expr[]) -> print args; printfn "")
    do addBinding "println" <| Runtime (fun (args: Value[]) -> print args; printf "\n")
    do addBinding "+" <| CompiledFn (fun (args: Value[]) ->
        let number = ref 0L
        for arg in args do
            match arg with
            // | Ident num -> number := number.Value + Int32.Parse(num)
            | Int int -> number := number.Value + int
            | Float f -> number := number.Value + int64 f
            | _ -> failwith "Invalid argument"
            // | ValueLiteral value
            // | value -> ()
            printfn $"Adding arg %A{arg}"
        Int number.Value)

    let toValue expr : obj =
        match expr with
        | Token.String str -> str.Replace("\\n", "\n")
        | _ -> box <| sprintf "%A" expr

    let rec parseToken (token: Token) : Expr =
        match token with
        | Token.Item ident -> Ident ident
        | Token.Expr tokens -> Call (parseToken tokens[0], tokens |> Array.tail |> Array.filter (function | Token.Comment _ -> false | _ -> true) |> Array.map parseToken)
        | Token.Comment _ -> failwith "No comments before parsing"
        | Token.String str -> ValueLiteral (String str)
        | Token.Number n -> ValueLiteral <| Int n
        | Token.Float d -> ValueLiteral <| Float d
        | Token.Quote q -> ValueLiteral <| Quote q
        | _ -> Unmatched token


    let rec evalAst (expr: Expr) : Value =
        match expr with
        | ValueLiteral value -> value
        | Unmatched value -> Runtime (Unmatched value)
        | Ident ident -> ns.bound.TryFind ident |> Option.defaultValue (Runtime None)
        | Def (ident, value) -> ns <- { ns with bound = ns.bound.Add (ident, value) }; Runtime null
        | Call (callee, args) ->
            let s = String.concat " " (Array.map (sprintf "%A") args)
            // todo: get screen size and print this off to the right
            printfn $"\t({callee} {s})"
            let value = evalAst callee
            match value with
            | Runtime value ->
                if FSharpType.IsFunction (value.GetType()) then
                    // printfn "Attempting to call function"
                    let (arg, returns) = FSharpType.GetFunctionElements (value.GetType())
                    let info = 
                        value.GetType().GetMethods() 
                        |> Array.find (fun i -> i.Name = "Invoke")
                    if arg = typeof<obj[]> && returns = typeof<unit> then
                        try
                            info.Invoke(value, [| (args |> Array.map evalAst) |])
                            |> Runtime
                        with error ->
                            printfn $"Error: {error}"
                            Runtime None
                    elif arg = typeof<Expr[]> && returns = typeof<unit> then
                        (info.Invoke(value, [| args |]))
                        |> Runtime
                    elif arg = typeof<Expr[]> && returns = typeof<System.Object> then
                        (info.Invoke(value, [| args |]))
                        |> Runtime
                    elif arg = typeof<Expr[]> then
                        (info.Invoke(value, [| args |]))
                        |> Runtime
                    elif arg = typeof<Value[]> then
                        let result = info.Invoke(value, [| args |> Array.map evalAst |])
                        Runtime result
                    else
                        printfn "Not sure how to call func: %A %A" arg returns
                        Runtime None
                else
                    Runtime None
                // printfn "Call %A with %A" callee args
                // null
            | CompiledFn valuesFunc ->
                valuesFunc (Array.map evalAst args)
            | value ->
                value

    let evalExpr (expr: Expr) =
        match expr with
        | anything -> null
    let rec eval (expr: Token) : obj =
        // printfn $"eval: %A{expr}"
        // if exprs.Length = 0 then failwith <| sprintf "Empty expression in %A" exprs
            // eval exprs
        match expr with
        | Token.Item ident when ns.bound.ContainsKey ident ->
            ns.bound[ident]
        | Token.Expr exprs -> 
            match exprs[0] with
            | Token.Item ident when ns.bound.ContainsKey ident ->
                // printfn $"found item in namespace: {ident} %A{ns.bound[ident]}"
                // printfn "%A %A" (ns.bound[ident]) (ns.bound[ident].GetType()) 
                let value = ns.bound[ident]

                if FSharpType.IsFunction (value.GetType()) then
                    // printfn "Attempting to call function"
                    let (arg, returns) = FSharpType.GetFunctionElements (value.GetType())
                    let info = 
                        value.GetType().GetMethods() 
                        |> Array.find (fun i -> i.Name = "Invoke")
                    if arg = typeof<obj[]> && returns = typeof<unit> then
                        try
                            info.Invoke(value, [| (Array.tail exprs) |> Array.map toValue |])
                        with error ->
                            printfn $"Error: {error}"
                            None
                    elif arg = typeof<Token[]> && returns = typeof<unit> then
                        Some (info.Invoke(value, [| Array.tail exprs |]))
                    elif arg = typeof<Token[]> && returns = typeof<System.Object> then
                        Some (info.Invoke(value, [| Array.tail exprs |]))
                    else
                        printfn "Not sure how to call func: %A %A" arg returns
                        None
                else
                    None
                    // printfn $"elements: %A{elements}"
                    // printfn "is function"
            | _ ->
                failwith "Can't evaluate other calls yet"
        | Token.String str -> str
        | Token.Quote q -> (box (sprintf "Quote %A" q))
        // | Token.Comment c -> printfn $";{c}"; None
        | expr -> (box <| sprintf "Unmatched expr %A" expr)
    let defn =
        ()
    do ns <- 
        evalRef <- eval
        {ns with
            // bound = ns.bound.Add ("defn", defn)
            bound = ns.bound.Add ("defn", Runtime (fun (tokens: Expr[]) -> 
                // let (Token.Item name) = tokens[0]
                // let (Token.Vector args) = tokens[1]
                let (Expr.Ident name) = tokens[0]
                let (Unmatched args) = tokens[1]
                let (Token.Vector args) = args :?> Token 
                let paramNames = args |> Array.map (fun (Token.Item name) -> name)
                // let paramNames = args |> Array.map (fun arg -> arg.ToString())
                let rest = tokens |> Array.skip 2
                ns <-
                 { ns with 
                    bound = ns.bound.Add(name, Runtime (fun (args: Value[]) -> 
                        let mutable existing = Map.empty
                        paramNames |> Array.iteri (fun index name ->
                            if ns.bound.ContainsKey name then existing <- existing.Add (name, ns.bound[name])
                            ns <- { ns with bound = ns.bound.Add(name, args[index]) } // |> toValue) }
                        )
                        let result = 
                            let mutable result = Runtime null
                            for item in rest do result <- evalAst item
                            result
                        paramNames |> Array.iteri (fun index name ->
                            ns <- { ns with bound = ns.bound.Remove(name) }
                        )
                        // Restore previous state of bindings
                        for kv in existing do ns <- { ns with bound = ns.bound.Add(kv.Key, kv.Value) }
                        // printfn "result: %A" result
                        ()))}
                printfn "defn %A" args))}
        
    do addBinding "eval" <| CompiledFn (fun (quote: Value[]) ->
            let (Quote quote) =
                if quote.Length <> 1 then failwith (sprintf "Invalid number of arguments given to eval.\n%A" quote)
                else quote[0]
            // evalAst (parseToken <| Token.Expr quote)
            evalAst (parseToken quote)
        )
    
    
    do addBinding "parse-text" <| CompiledFn (fun (text: Value[]) ->
        let (String text) =
            if text.Length <> 1 then failwith (sprintf "Invalid number of arguments given to parse-text.\n%A" quote)
            else text[0]
        match FParsec.CharParsers.run Parser.expr text with
        | FParsec.CharParsers.ParserResult.Success (tokens, _, _) -> Quote tokens
        | _ -> failwith ""
    )