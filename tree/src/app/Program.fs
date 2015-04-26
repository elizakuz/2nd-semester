module tree

type Tree = Nil | T of Tree * int * Tree

type TreeBuilder () =
    member this.Bind (x, f) =
        match x with
        | Nil -> Nil
        | T (a, b, c) -> T (this.Bind (a, f), f b, this.Bind (c, f))
    member this.Combine (x, y) =
        match x, y with
        | Nil, _ -> y
        | _, Nil -> x  
        | T (a, b, c), T (k, l, m) -> if b < l then T (a, b, this.Combine (c, y))
                                      else T (this.Combine (x, k), l, m)
    member this.Return x = 
        T (Nil, x, Nil)
    member this.ReturnFrom x = x
    member this.For (x, f) = this.Bind (x, f)

let tree = new TreeBuilder()

let map f tr =
    tree {
        for x in tr do
             return! f x
    }

let rec filter f tr =
    tree {
        match tr with
        | Nil -> return! Nil
        | T (a, b, c) -> if (f b) then return! T (filter f a, b, filter f c) 
                         else return! filter f a
    }

[<EntryPoint>]
let main argv =
    0      