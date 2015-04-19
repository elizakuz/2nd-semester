module exTree

open System
type exTree = Op of string * exTree * exTree * bool | Num of float | Mut of string | Nil

let exList (list : list<string>) =
    let mutable l = []
    for i = 0 to list.Length - 1 do
        match list.[i] with
        | x when x = "+" || x = "-" || x = "^" || x = "%" || x = "/" || x = "*" || x = "=" ->
            l <- Op (x, Nil, Nil, true) :: l 
        | y -> let mutable m = false
               for k = 0 to y.Length - 1 do
                   if not (Char.IsDigit y.[k]) then m <- true 
               if m then l <- Mut y :: l
               else l <- Num (System.Convert.ToDouble (System.Convert.ToInt32 y)) :: l
    List.rev l

let rec makeExTree (list : exTree list) : exTree =
    let mutable ar = List.toArray list
    let mutable num = ar.Length
    for i = 0 to ar.Length - 1 do
        match ar.[i] with 
        | Op (a, b, c, true) -> num <- i
        | _ -> ()
    if num <> ar.Length then 
        match ar.[num] with
        | Op (a, b, c, d) -> ar.[num] <- Op (a, ar.[num + 1], ar.[num + 2], false)
                             ar <- Array.append ar.[0..num] ar.[num + 3..ar.Length - 1]
                             makeExTree (Array.toList ar)
        | _ -> ar.[0]
    else ar.[0]

let math (zn : string, v1: float, v3 : float) = 
    match zn with 
    | "^" -> v1 ** v3
    | "*" -> v1 * v3
    | "/" -> v1 / v3
    | "%" -> v1 % v3
    | "+" -> v1 + v3
    | "-" -> v1 - v3
    | _ -> 0.0

let rec exObhod exTr (ar : array<string*double>) =
    match exTr with
    | Mut x ->  let mutable res = 0.0
                for i = 0 to ar.Length - 1 do
                   match ar.[i] with 
                   | (s, d) -> if s = x then res <- d
                res
    | Num x -> x
    | Nil -> 0.0
    | Op (a, b, c, d) -> match b, c with 
                           | Num x, Num y -> math (a, x, y)
                           | Num x, _ -> math (a, x, exObhod c ar)
                           | _, Num x -> math (a, exObhod b ar, x)
                           | _, _ -> math (a, exObhod b ar, exObhod c ar)


