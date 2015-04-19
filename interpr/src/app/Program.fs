module progr
open mainTr
open exTree
open parser
open System.IO


let CLRcons (der, ch) =
  let mutable ar : array<string*double> = Array.create ch ("",0.0)
  let rec Obhod tr (ar : array<string*double>) =
    match tr with
    | Read a -> printfn "Enter the number"
                let mutable c = 0
                let mutable num = ar.Length
                while c < ar.Length do
                    match ar.[c] with
                    | (r, _) when r = a -> num <- c; c <- ar.Length
                    | ("", 0.0) -> num <- c; c <- ar.Length
                    | (_, _) -> c <- c + 1
                ar.[num] <- (a,double (System.Console.ReadLine()))
    | Write a -> printfn "%A" (exObhod a ar)
    | Assign (a, b) -> let mutable c = 0
                       let mutable num = ar.Length
                       while c < ar.Length do
                           match ar.[c] with
                           | (r, _) when r = a -> num <- c; c <- ar.Length
                           | ("", 0.0) -> num <- c; c <- ar.Length
                           | (_, _) -> c <- c + 1
                       ar.[num] <- (a,exObhod b ar) 
    | Seq (a, b) -> Obhod a ar
                    Obhod b ar
    | If (a, b, c) -> if exObhod a ar > 0.0 then Obhod b ar
                      else Obhod c ar
    | While (a, b) -> while exObhod a ar > 0.0 do
                          Obhod b ar
    | _ -> ()                                     
  Obhod der ar

let CLRfile (der, ch, array) =
  use stream = new StreamWriter ("to.txt")
  let mutable ar : array<string*double> = Array.create ch ("",0.0)
  let rec Obhod tr (ar : array<string*double>) (s : double array) =
    match tr with
    | Read a -> let mutable c = 0
                let mutable num = ar.Length
                while c < ar.Length do
                    match ar.[c] with
                    | (r, _) when r = a -> num <- c; c <- ar.Length
                    | ("", 0.0) -> num <- c; c <- ar.Length
                    | (_, _) -> c <- c + 1
                ar.[num] <- (a, s.[0])
                for i = 0 to s.Length - 2 do
                    s.[i] <- s.[i+1]
    | Write a -> stream.WriteLine (exObhod a ar)
    | Assign (a, b) -> let mutable c = 0
                       let mutable num = ar.Length
                       while c < ar.Length do
                           match ar.[c] with
                           | (r, _) when r = a -> num <- c; c <- ar.Length
                           | ("", 0.0) -> num <- c; c <- ar.Length
                           | (_, _) -> c <- c + 1
                       ar.[num] <- (a,exObhod b ar) 
    | Seq (a, b) -> Obhod a ar s
                    Obhod b ar s
    | If (a, b, c) -> if exObhod a ar > 0.0 then Obhod b ar s
                      else Obhod c ar s
    | While (a, b) -> while exObhod a ar > 0.0 do
                          Obhod b ar s
    | _ -> ()                                     
  Obhod der ar array


