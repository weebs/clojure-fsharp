module Unit.Tests.Console

open System
open System.Linq.Expressions
open FSharp.Quotations.Evaluator
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers

let inline eval (e: 't Expr) = LeafExpressionConverter.EvaluateQuotation e :?> 't
let inline compile (e: 't Expr) = LeafExpressionConverter.QuotationToLambdaExpression e

let main () =
    try
        printfn "1234"
        printfn "LINQ"
        // let i = Mono.Cecil.
        // printfn "%A" <| (eval <@ let f x y = x + y in f @>)
        let t = (<@ let f x y = x + y in f @>).Compile()
        let m =
            t.GetType().GetMethods()
            |> Array.find (fun m -> m.Name = "Invoke")
        printfn "%A" (m.GetMethodBody().GetILAsByteArray())
        printfn "m = %A" <| m.Invoke(t, [| 420; 1337 |])

        printfn "ope"
        //
        // printfn "%A" <| (<@ (fun () -> 1 + 1) @>).Compile()
        // let f = (<@ fun a b -> a + b @>).Compile
        // let e = <@ 2 + 1 @>
        // printfn "e = %A" <| <@ 1 + %e @>.Compile()
        // printfn "lambda = %A" <| ((Expression.Lambda (<@ fun n -> n + 1 @>.ToLinqExpressionUntyped())).Compile().DynamicInvoke() :?> Delegate)
        // // printfn "hehehe %A" <| (compile <@ let f x y = x + y in f @>)
        // printfn "%A" <| (compile <@ Func<_,_>(fun () -> 1 + 2) @>)
    with error -> printfn "%A" error
    
