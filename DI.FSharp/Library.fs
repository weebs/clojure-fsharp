module DI

open System.Collections.Generic

type FileWatch = { task: obj; cancel: bool ref }
let fullName<'t> () =
    let n = typeof<'t>.FullName
    if n.StartsWith "FSI_" then n.Substring(n.IndexOf(".") + 1)
    else n
    
    
type Dependency() =
    static let items = Dictionary<string, obj>()
    static let namespaces = Dictionary<string * string, obj>()
    static member resolve<'t> () =
        try items[fullName<'t> ()] :?> 't |> Some with error -> None
    static member resolve<'t> (name: string) =
        try namespaces[fullName<'t> (), name] :?> 't |> Some with error -> None
    static member define<'t> (t: 't) =
        items[fullName<'t> ()] <- t
        t
    static member define<'t> (t: 't, name: string) =
        namespaces[(fullName<'t> (), name)] <- t
        t
    static member defineOnce<'t> (f: unit -> 't) =
        if not (items.ContainsKey <| fullName<'t> ()) then
            items[fullName<'t> ()] <- f ()
            Some (items[fullName<'t> ()] :?> 't)
        else None
    static member repl<'t> (f: unit -> 't) =
        if not (items.ContainsKey <| fullName<'t> ()) then
            items[fullName<'t> ()] <- f ()
            None
        else
            let old = items[fullName<'t> ()] :?> 't
            items[fullName<'t> ()] <- f ()
            Some old
    static member defineOnce<'t> (f: string -> 't, name: string) =
        namespaces[(fullName<'t> (), name)] <- f name
        namespaces[(fullName<'t> (), name)] :?> 't
