module Clojure.Parser

open FParsec
open System
open System.Text
open FParsec
open FSharp.Data.LiteralProviders
open FSharp.Reflection
// open System

// module Clojure =
type Value =
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
    | Metadata of Value
    | Map of (Value * Value)[]
    | HashSet of Value[]
    | NamespaceIdent of string
// type CompiledValue =
    // | Value of Value
    | MacroDefn of (Value[] -> Value)
    | CompiledFn of (Value[] -> Value)
    | Builtin of (Value[] -> Value)
    override this.ToString() =
        match this with
        | Number n -> string n
        | Float f -> string f
        | String s -> s
        | _else -> _else.ToString()
module Parser =
    let ident =
        
        many1Chars (noneOf [ ';'; '('; ')'; '`'; '~'; ' '; '"'; '''; '#'; '\n'; '['; ']'; '{'; '}'; ',' ])
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

    let metadata = pchar '^' >>. value |>> Metadata
    let hashtagFunc = pchar '#' .>> pchar '(' >>. innerExprList .>> pchar ')' |>> InlineFunc
    let map = pchar '{' >>. many ((string <|> keyword) .>> optional spaces .>>. value .>> optional (pchar ',') .>> optional spaces) .>> pchar '}' |>> (List.toArray >> Map) 
    let hashset = pchar '#' >>. pchar '{' >>. innerExprList .>> pchar '}' |>> HashSet
    let booleanLiteral = (pstring "true" |>> fun _ -> Boolean true) <|> (pstring "false" |>> fun _ -> Boolean false)
    // let namespaceIdent = pchar ''' >>. ident |>> NamespaceIdent

    let floatingPoint = many digit .>>. attempt (pchar '.') .>>. many digit |>> fun ((nums, decPt), dec) ->
        let nums = String.concat "" (List.map (fun c -> c.ToString()) nums)
        let dec = String.concat "" (List.map (fun c -> c.ToString()) dec)
        printfn $"result = {nums}{decPt}{dec}"
        Double.Parse($"{nums}{decPt}{dec}") |> Float
    valueRef := 
        (comment <|> (attempt floatingPoint) <|> (pint64 |>> Number) <|> booleanLiteral <|>
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
    let file = manyTill ((optional spaces >>. optional (skipMany comment) >>. ((attempt comment) <|> expr <|> (optional eof |>> fun _ -> Comment "EOF")) |>> Some .>> optional spaces)) eof |>> List.choose id
        // manyCharsTill anyChar (parenthesis <|> (pchar ')' |>> fun _ -> [])) .>> pchar ')' |>> fun foo -> Tokens text :: items

    // this can fix the type errors
    match run file "" with
    | _ -> ()