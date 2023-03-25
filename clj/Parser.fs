module Clojure.Parser
open FParsec
open System
open System.Text
open FParsec
open FSharp.Data.LiteralProviders
open FSharp.Reflection
// open System

// module Clojure =
type Token =
    | Expr of Token[]
    | Item of string
    | String of string
    | Comment of string
    | Vector of Token[]
    | Number of int64
    | Float of double
    | Keyword of string
    | Quote of Token

    | InlineFunc of Token[]
    | Map of (Token * Token)[]
    | HashSet of Token[]
    | NamespaceIdent of string
let ident =
    many1Chars (noneOf [ ';'; '('; ')'; ' '; '"'; '''; '#'; '\n'; '['; ']'; '{'; '}'; ',' ])
let keyword = pchar ':' >>. ident |>> Keyword
let string = pchar '"' >>. manyChars (noneOf [ '"' ]) .>> pchar '"' |>> String
let token = ident |>> Item
let (innerExprList, innerExprListRef) : Parser<Token[], _> * _ = createParserForwardedToRef ()
let (value, valueRef) : Parser<Token, _> * _ = createParserForwardedToRef ()
let expr = pchar '(' >>. innerExprList .>> pchar ')' |>> Expr
let vector = pchar '[' >>. innerExprList .>> pchar ']' |>> Vector
let comment = 
    pchar ';' >>. 
    // manyChars (noneOf [ '\n'; ]) 
    manyCharsTill anyChar ((newline |>> fun _ -> ()) <|> eof)
    |>> Comment
// let quote = pchar ''' >>. pchar '(' >>. innerExprList .>> pchar ')' |>> Quote
let quote = pchar ''' >>. expr |>> Quote
let hashtagFunc = pchar '#' .>> pchar '(' >>. innerExprList .>> pchar ')' |>> InlineFunc
let map = pchar '{' >>. many ((string <|> keyword) .>> optional spaces .>>. value .>> optional (pchar ',') .>> optional spaces) .>> pchar '}' |>> (List.toArray >> Map) 
let hashset = pchar '#' >>. pchar '{' >>. innerExprList .>> pchar '}' |>> HashSet
let namespaceIdent = pchar ''' >>. ident |>> NamespaceIdent
valueRef := (comment <|> (pint64 |>> Number) <|> (pfloat |>> Float) <|> (attempt namespaceIdent) <|> token <|> string <|> keyword <|> expr <|> vector <|> quote <|> hashtagFunc <|> map <|> hashset) .>> optional spaces
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