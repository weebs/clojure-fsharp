module Clojure.Read

open System.Collections.Generic
open System.Runtime.CompilerServices
open System
open System.Text
open System.Threading
open FSharp.Data.LiteralProviders
open FSharp.Reflection

type Atom<'t when 't : equality and 't : not struct> = private { mutable value: 't } with

    static member Init value = { value = value }
    static member get a = a.value
    static member set (a: Atom<'t>) (value: 't) =
        let rec loop (original: 't) =
            if original = Interlocked.CompareExchange<'t> (&a.value, value, original) then ()
            else loop a.value
        loop a.value
    // static member set (a: Atom<'t>) (value: 't) =
    //     Atom.Set this value
    static member Swap (a: Atom<'t>) (updater: 't -> 't) =
        let rec loop (original: 't) =
            if original = Interlocked.CompareExchange<'t> (&a.value, updater a.value, original) then ()
            else loop a.value
        loop a.value
    member this.swap (updater: 't -> 't) =
        Atom.Swap this updater


and Value =
    | Null
    | TokenList of Value[]
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
    | Map of Dictionary<Value, Value>
    | HashSet of Value[]
    | NamespaceIdent of string
    | MacroDefn of obj
    | CompiledFn of obj
    | ClojureForm of obj
    | Atom of Atom<Value>
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
        | _else -> sprintf "%A" this
        



type ClojureType =
    | Class of name: string * fields: (string * string)[]
    | Record of name: string * fields: (string * string)[]
    
type Namespace = { name: string; imports: obj; bound: Map<string, Value> }
    
let metdataTable = ConditionalWeakTable<Value, Value>()
module Parser =
    open FParsec
    let ident =
        many1Chars (noneOf [ ':'; ';'; '('; ')'; '`'; '~'; ' '; '"'; '''; '#'; '\n'; '['; ']'; '{'; '}'; ',' ])
    let deref = pchar '@' >>. ident |>> fun ident -> TokenList [| Symbol "deref"; Symbol ident |]
    let keyword = pchar ':' >>. ident |>> Keyword
    let string = pchar '"' >>. manyChars (noneOf [ '"' ]) .>> pchar '"' |>> String
    let token = ident |>> Symbol
    let (innerExprList, innerExprListRef) : Parser<Value[], _> * _ = createParserForwardedToRef ()
    let (value, valueRef) : Parser<Value, _> * _ = createParserForwardedToRef ()
    let expr = pchar '(' >>. innerExprList .>> pchar ')' |>> TokenList
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
        metdataTable.Add(value, metadata)
        value
    let hashtagFunc = pchar '#' .>> pchar '(' >>. innerExprList .>> pchar ')' |>> InlineFunc
    let map = pchar '{' >>. many ((string <|> keyword) .>> optional spaces .>>. value .>> optional (pchar ',') .>> optional spaces) .>> pchar '}' |>> fun values ->
        Map.ofList [
            for (key, value) in values do
                match key with
                | Value.Keyword key
                | Value.String key -> yield key, value
        ] |> Map
    let hashset = pchar '#' >>. pchar '{' >>. innerExprList .>> pchar '}' |>> HashSet
    let booleanLiteral = (pstring "true" |>> fun _ -> Boolean true) <|> (pstring "false" |>> fun _ -> Boolean false)
    // let namespaceIdent = pchar ''' >>. ident |>> NamespaceIdent

    let floatingPoint = many1 digit .>>. attempt (pchar '.') .>>. many digit |>> fun ((nums, decPt), dec) ->
        let nums = String.concat "" (List.map (fun c -> c.ToString()) nums)
        let dec = String.concat "" (List.map (fun c -> c.ToString()) dec)
        Double.Parse($"{nums}{decPt}{dec}") |> Float
    valueRef := 
        (comment <|> (attempt floatingPoint) <|> (pint64 |>> Number) <|> booleanLiteral <|>
         metadata <|>
         deref <|>
         nil <|>
         token <|> string <|>
         keyword <|> expr <|> vector <|> quote <|> hashtagFunc <|> map <|>
         hashset <|> quoteSyntax <|> unquote <|> unquoteSplice) .>> optional spaces
    innerExprListRef :=
        // optional spaces >>. pchar '(' >>. many (token <|> string <|> parenthesis) .>> manyChars (noneOf [ ')' ]) .>> pchar ')' |>> (List.toArray >> Expr)
        optional spaces >>. 
        // pchar '(' >>. 
        many value .>> 
        // manyChars (noneOf [ ')' ]) .>> pchar ')' .>> 
        optional spaces
        |>> List.toArray
    let file = manyTill ((optional spaces >>. optional (skipMany comment) >>. ((attempt comment .>>. getPosition) <|> (expr .>>. getPosition) <|> (optional eof .>>. getPosition |>> fun (_, p) -> Comment "EOF", p)) |>> Some .>> optional spaces)) eof |>> List.choose id
        // manyCharsTill anyChar (parenthesis <|> (pchar ')' |>> fun _ -> [])) .>> pchar ')' |>> fun foo -> Tokens text :: items

    // this can fix the type errors
    match run file "" with
    | _ -> ()