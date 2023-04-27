module Clojure.Read

open System.Collections.Generic
// open System.Collections.Immutable
open System.Runtime.CompilerServices
open System
open System.Text
open System.Threading
open FSharp.Interop.Dynamic
    
// open FSharp.Data.LiteralProviders
open FSharp.Reflection
// type Atom<'t when 't : equality and 't : not struct> = private { mutable value: 't } with
type Atom<'t when 't : not struct> = private { mutable value: 't } with

    static member set (a: Atom<'t>) (value: 't) =
        let rec loop (original: 't) =
            let value = Interlocked.CompareExchange(&a.value, value, original)
            if (original :> obj) = (value :> obj) then ()
            else loop a.value
        loop a.value
    static member Init value = { value = value }
    static member get a = a.value
    // static member set (a: Atom<'t>) (value: 't) =
    //     Atom.Set this value
    static member Swap (a: Atom<'t>) (updater: 't -> 't) =
        let rec loop (original: 't) =
            if (original :> obj) = (Interlocked.CompareExchange<'t> (&a.value, updater a.value, original) :> obj) then ()
            else loop a.value
        loop a.value
    member this.swap (updater: 't -> 't) =
        Atom.Swap this updater


and Value =
    | Null
    | List of Value[]
    | Symbol of string
    | String of string
    | Comment of string
    | Vector of Value[]
    | Number of int64
    | Float of double
    | Boolean of bool
    | Keyword of string
    | Quote of Value
    | QuoteSyntax of Value
    | Unquote of Value
    | UnquoteSplice of Value
    | InlineFunc of Value[]
    // | Metadata of Value * Value
    // | Map of Dictionary<Value, Value>
    // | Map of ImmutableDictionary<string, Value>
    | Map of Map<string, Value>
    | HashSet of Value[]
    | NamespaceIdent of string
    | MacroDefn of (Value[] -> Value)
    | CompiledFn of (Value[] -> Value)
    | ExprEvaluator of (Value[] -> Value)
    //| ExprEvaluator of obj
    | Atom of Atom<Value>
    | Sequence of seq<Value>
    // | NonComparableValue of NonComparableValue
    // | MacroDefn of (Value[] -> Value)
    // | CompiledFn of (Value[] -> Value)
    // | ClojureForm of (Value[] -> Value)
    | Obj of obj
    override this.ToString() =
        match this with
        | Number n -> string n
        | Float f -> string f
        | String s -> s
        | Keyword k -> ":" + k
        | Map m ->
            let keyvalues = String.concat " " (seq {
                for kv in m do yield $"{kv.Key} {kv.Value}"
                // for kv in m.Seq do yield (sprintf "%s %A" kv.Key kv.Value)
            })
            sprintf "{%s}" keyvalues
            // $"{{{keyvalues}}}"
        | _else -> sprintf "%A" this
        



type ClojureType =
    | Class of name: string * fields: (string * string)[]
    | Record of name: string * fields: (string * string)[]
    
type Namespace = { name: string; imports: obj; bound: Map<string, Value> }
    
let varDataTable = ConditionalWeakTable<Value, Value>()
module Parser =
    open FParsec
    let tryMany p =
        fun stream ->
            let mutable result = (Primitives.attempt p) stream
            let mutable n = result.Error
            if n <> null then Reply([])
            else
                let items =
                    [
                        while result.Status = Ok && result.Error = n do
                            n <- result.Error
                            yield result.Result
                            result <- (Primitives.attempt p) stream
                    ] @ [
                        if result.Status = Ok && result.Error <> n then
                            yield result.Result
                    ]
                Reply(Ok, items, null)
    let ident =
        many1Chars (noneOf [ ':'; ';'; '('; ')'; '`'; '~'; ' '; '"'; '''; '#'; '\n'; '['; ']'; '{'; '}'; ',' ])
    let deref = pchar '@' >>. ident |>> fun ident -> List [| Symbol "deref"; Symbol ident |]
    let keyword = pchar ':' >>. ident |>> Keyword
    let string = pchar '"' >>. manyChars (noneOf [ '"' ]) .>> pchar '"' |>> fun text ->
        let indexes = [|
            for i in 0..(text.Length - 2) do
                if text.[i] = char "\\" && text.[i + 1] = 'u' then yield i
        |]
        // Convert \uNNNN base16 to unicode
        let convertParts (separator: string) (length: int) (_base: int) (text: string) =
            let parts = text.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)
            let parts = [|
                yield parts.[0]
                for i in 1..(parts.Length - 1) do
                    yield
                        string (Convert.ToChar(Convert.ToInt32(parts.[i].Substring(0, length), _base)))
                        + parts.[i].Substring(length, parts.[i].Length - length)
            |]
            (String.concat "" parts)
        text
        |> convertParts "\\u" 4 16
        |> convertParts "\\x" 2 16
        |> String       
    let token = ident |>> Symbol
    let (innerExprList, innerExprListRef) : Parser<Value[], _> * _ = createParserForwardedToRef ()
    let (value, valueRef) : Parser<Value, _> * _ = createParserForwardedToRef ()
    let expr = pchar '(' >>. innerExprList .>> pchar ')' |>> List
    // let expr = pchar '(' >>. (manyTill value (pchar ')')) |>> (Array.ofList >> List)
    // let expr = between (pchar '(') (pchar ')') value
    let vector = pchar '[' >>. innerExprList .>> pchar ']' |>> Vector
    let comment = 
        pchar ';' >>. 
        // manyChars (noneOf [ '\n'; ]) 
        manyCharsTill anyChar ((newline |>> fun _ -> ()) <|> eof)
        |>> Comment
    let unquote = pchar '~' >>. value |>> Unquote
    let quoteSyntax = pchar '`' >>. value |>> QuoteSyntax
    let unquoteSplice = pstring "~@" >>. value |>> UnquoteSplice
    let quote = pchar ''' >>. value |>> Quote
    let nil = pstring "nil" |>> fun _ -> Null
    let metadata = pchar '^' >>. value .>>. value |>> fun (metadata, value) ->
        varDataTable.Add(value, metadata)
        value
    let hashtagFunc = pchar '#' .>> pchar '(' >>.  innerExprList .>> pchar ')' |>> InlineFunc
    let map = pchar '{' >>. (optional spaces) >>. many ((string <|> keyword) .>> optional spaces .>>. value .>> optional (pchar ',') .>> optional spaces) .>> pchar '}' |>> fun values ->
       // let result =
       //     (Dictionary<_,_>(), Map.ofList [
       //          for (key, value) in values do
       //              match key with
       //              | Value.Keyword key
       //              | Value.String key -> yield key, value
       //      ] |> Map.toArray) ||> Array.fold (fun dictionary (key, item) -> dictionary.Add(Keyword key, item); dictionary) |> Map
       let values = [|
           yield Symbol "hash-map"
           for (key, value) in values do
               match key with
               | Value.Keyword _
               | Value.String _ ->
                   yield key
               yield value
       |]
       List values
    let hashset = pchar '#' >>. pchar '{' >>. innerExprList .>> pchar '}' |>> HashSet
    let booleanLiteral = (pstring "true" |>> fun _ -> Boolean true) <|> (pstring "false" |>> fun _ -> Boolean false)
    // let namespaceIdent = pchar ''' >>. ident |>> NamespaceIdent

    let floatingPoint = attempt (pstring "-" <|> (fun stream -> Reply(""))) .>>. many1 digit .>>. attempt (pchar '.') .>>. many digit |>> fun (((sign, nums), decPt), dec) ->
        let nums = String.concat "" (List.map (fun c -> c.ToString()) nums)
        let dec = String.concat "" (List.map (fun c -> c.ToString()) dec)
        // Double.Parse($"{sign}{nums}{decPt}{dec}") |> Float
        Double.Parse(sign + nums + decPt.ToString() + dec) |> Float
    // let (<|>) (p1: Parser<_,_>) p2 =
    //     fun (stream: CharStream<_>) ->
    //         let mutable stateTag = stream.StateTag
    //         let mutable reply = p1 stream
    //         if reply.Status = Error && stateTag = stream.StateTag then
    //             let error = reply.Error
    //             reply <- p2 stream
    //             if stateTag = stream.StateTag then
    //                 reply.Error <- mergeErrors reply.Error error
    //         reply
    let DEBUG = false
    let (>>!) (p1: Parser<_,_>) (msg: 'a) =
        if DEBUG then
            fun (stream: CharStream<_>) ->
                printfn "%A" msg
                let line = stream.Line
                let col = stream.Column
                let reply = p1 stream
                if reply.Status = Error then
                    ()
                elif reply.Status = Ok then
                    printfn "parsed %A %d %d" msg (stream.Line) (stream.Column)
                elif reply.Status = FatalError then
                    printfn "Exiting"
                reply
        else p1
    let tryParse p =
        printfn "attempting %A" p
        let mutable result = Primitives.attempt p
        fun stream ->
            let result = result stream
            printfn "%A" result
            if result.Status = Error then
                // result.Status <- Ok
                Reply(Ok, result.Result, result.Error)
            else
                result
    valueRef :=
        // (fun stream ->
        //     printfn ""
        //     Reply("")
        //     ) >>.
        // optional comment >>.
        // optional spaces >>.
        (tryMany (comment |>> ignore <|> spaces)) >>.
        (
            ((attempt floatingPoint) >>! "floatingPoint") <|>
            (attempt (pint64 >>! "pint64" |>> Number)) <|>
            (booleanLiteral >>! "booleanLiteral") <|>
            (metadata >>! "metadata") <|>
            (deref >>! "deref") <|>
            (nil >>! "nil") <|>
            (token >>! "token") <|>
            (string >>! "string") <|>
            (keyword >>! "keyword") <|>
            (expr >>! "expr") <|>
            (vector >>! "vector") <|>
            (quote >>! "quote") <|>
            (hashtagFunc >>! "hashtagFunc") <|>
            (map >>! "map") <|>
            (hashset >>! "hashset") <|>
            (quoteSyntax >>! "quoteSyntax") <|>
            (unquote >>! "unquote") <|>
            (unquoteSplice >>! "unquoteSplice")
        )
        //.>> //(optional spaces)
        .>> (tryMany (comment |>> ignore <|> spaces))
    innerExprListRef :=
        // optional spaces >>. pchar '(' >>. many (token <|> string <|> parenthesis) .>> manyChars (noneOf [ ')' ]) .>> pchar ')' |>> (List.toArray >> Expr)
        
        // optional spaces >>.
        
        // pchar '(' >>.
        
        many value //.>> 
        // manyChars (noneOf [ ')' ]) .>> pchar ')' .>>
        
        // optional spaces
        
        |>> fun items -> items |> List.filter (function Comment _ -> false | _ -> true ) |> List.toArray
    let file =
        manyTill (
            (optional spaces >>! "spaces") >>.
            // (optional (skipMany comment) >>! "skipping comments") >>.
            (
                ((getPosition .>>. (optional comment) .>>. getPosition) >>! "skipping single comment") >>. (
                ((getPosition .>>. expr .>>. getPosition) >>! "reading expr") <|>
                (getPosition .>>. optional eof .>>. getPosition >>! "trying EOF" |>> fun ((start, _), p) ->
                    ((start, Comment "EOF"), p))
                |>> Some .>>
                optional spaces
            ))
        )
            eof
        |>> List.choose id
        // manyCharsTill anyChar (parenthesis <|> (pchar ')' |>> fun _ -> [])) .>> pchar ')' |>> fun foo -> Tokens text :: items

    // this can fix the type errors
    match run file "" with
    | _ -> ()
module Dlr =
    let dlrTest () =
        let dynamic = Dynamic.ExpandoObject()
        Dyn.set "foo" dynamic 1
        printfn "%A" <| Dyn.get "foo" dynamic
