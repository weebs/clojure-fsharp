module clojure.core

open System
open Clojure.Read


let println (values: Value[]) =
    let s = String.concat " " (Array.map string values)
    Console.WriteLine(s)
    Value.Null
    
let printfn (values: Value[]) =
    Console.WriteLine(String.concat " " (Array.map (sprintf "%A") values))
type Op<'a, 'b> =
    | Eq of ('a * 'b) * Return<bool>
and Return<'t> = 't -> ReturnValue
and ReturnValue = private { value: obj; typ: string } with
    member this.Value = ()
    static member Foo = ()
module Math =
    let gt (values: Value[]) : Value =
        // TODO GADT returns / ops in AST
        match values.[0], values.[1] with
        | Value.Number n, Value.Number n2 -> (n > n2)
        | Value.Number n, Value.Float n2 -> (float n > n2)
        | Value.Float n, Value.Number n2 -> (float n > float n2)
        | Value.Float n, Value.Float n2 -> (n > n2)
        | Value.String s, Value.String s2 -> (s > s2)
        |> Value.Boolean
        
    let lt (values: Value[]) : Value =
        match values.[0], values.[1] with
        | Value.Number n, Value.Number n2 -> (n < n2)
        | Value.Number n, Value.Float n2 -> (float n < n2)
        | Value.Float n, Value.Number n2 -> (float n < float n2)
        | Value.Float n, Value.Float n2 -> (n < n2)
        | Value.String s, Value.String s2 -> (s < s2)
        |> Value.Boolean
        
        
        
    let abs = CompiledFn <| fun (values: Value[]) ->
        match values.[0] with
        | Number n -> Number <| Math.Abs(n)
        | Float f -> Float <| Math.Abs(f)
    let _mod (values: Value[]) : Value =
        match values.[0], values.[1] with
        | Value.Number n, Value.Number n2 -> n % n2 |> Number
        | Value.Number n, Value.Float n2 -> float n % n2 |> Float
        | Value.Float n, Value.Number n2 -> n % float n2 |> Float
        | Value.Float n, Value.Float n2 -> float n % n2 |> Float
        
    let subtract (values: Value[]) : Value =
        if values.Length = 1 then
            match values.[0] with
            | Value.Number i -> Value.Number <| i * -1L
            | Value.Float f -> Value.Float <| f * -1.0
        else
            let mutable n_int = 0L
            let mutable n = 0.0
            let mutable isFloat = false
            match values.[0] with
            | Value.Number i -> n_int <- i
            | Value.Float f ->
                isFloat <- true
                n <- f
            for value in (values |> Array.skip 1) do
                match value with
                | Value.Number i -> if isFloat then n <- n - double i else n_int <- n_int - i
                | Value.Float f ->
                    if not isFloat then
                        isFloat <- true
                        n <- double n_int
                    n <- n - f
                | _ -> failwithf "Can't subtract value %A\n%A" value Environment.StackTrace
            if isFloat then Value.Float n else Value.Number n_int
    
    let mult (values: Value[]) : Value =
        let mutable n_int = 1L
        let mutable n = 1.0
        let mutable isFloat = false
        if values.Length = 1 then
            match values.[0] with
            | Value.Number i -> Value.Number <| i * n_int
            | Value.Float f -> Value.Float <| f * n
        else
            match values.[0] with
            | Value.Number i -> n_int <- i
            | Value.Float f ->
                isFloat <- true
                n <- f
            for value in (values |> Array.skip 1) do
                match value with
                | Value.Number i -> if isFloat then n <- n * double i else n_int <- n_int * i
                | Value.Float f ->
                    if not isFloat then
                        isFloat <- true
                        n <- double n_int
                    n <- n * f
                | _ -> failwithf "Can't subtract value %A\n%A" value Environment.StackTrace
            if isFloat then Value.Float n else Value.Number n_int
    // let division (values: Value[]) : Value =
    //     let mutable n_int = 1L
    //     let mutable n = 1.0
    //     let mutable isFloat = false
    //     if values.Length = 1 then
    //         match values[0] with
    //         | Value.Number i -> Value.Number <| n_int / i
    //         | Value.Float f -> Value.Float <| n / f
    //     else
    //         match values[0] with
    //         | Value.Number i -> n_int <- i
    //         | Value.Float f ->
    //             isFloat <- true
    //             n <- f
    //         for value in (values |> Array.skip 1) do
    //             match value with
    //             | Value.Number i -> if isFloat then n <- n / double i else n_int <- n_int / i
    //             | Value.Float f ->
    //                 if not isFloat then
    //                     isFloat <- true
    //                     n <- double n_int
    //                 n <- n / f
    //             | _ -> failwithf "Can't divide value %A\n%A" value Environment.StackTrace
    //         if isFloat then Value.Float n else Value.Number n_int
    let division (values: Value[]) : Value =
        let mutable n = 0.0
        match values.[0] with
        | Value.Number i -> n <- float i
        | Value.Float f -> n <- f
        for value in (values |> Array.skip 1) do
            match value with
            | Value.Number i -> n <- n / float i
            | Value.Float f -> n <- n / f
            | _ -> failwithf "Can't divide value %A\n%A" value Environment.StackTrace
        Float n
