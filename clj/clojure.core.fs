module clojure.core

open System
open Clojure

let println (values: Value[]) =
    let s = String.concat " " (Array.map string values)
    Console.WriteLine(s)
    Value.Null
    
module Math =
    let subtract (values: Value[]) : Value =
        if values.Length = 1 then
            match values[0] with
            | Value.Number i -> Value.Number <| i * -1L
            | Value.Float f -> Value.Float <| f * -1.0
        else
            let mutable n_int = 0L
            let mutable n = 0.0
            let mutable isFloat = false
            match values[0] with
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