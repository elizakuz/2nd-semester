module Modul

type ModulBuilder (n : int) =
    member this.Bind (m, f) = f (m % n)
    member this.Return x =
        let rec f y =
            if y < 0 then f (y + n)
            else y
        f (x % n)

let modul x = new ModulBuilder (x)

[<EntryPoint>]
let main argv =
    0