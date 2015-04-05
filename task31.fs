(* Task 31 
   Time: expectation 0.5h
         reality 1.5h
      Kuzmina Elizaveta *)
open NUnit.Framework

type IGraph =
  abstract member Size : int
  abstract member Vertices : int list 
  abstract member Edge : int -> int -> bool

type Graph (v : int list, matrix : bool [,]) =
  interface IGraph with
    member this.Size = v.Length
    member this.Vertices = v 
    member this.Edge v1 v2 = matrix.[v1, v2]

type GraphList (v : int list, list : (int list) list) =
  interface IGraph with
    member this.Size = v.Length
    member this.Vertices = v
    member this.Edge v1 v2 = (List.filter (fun x -> x = v2) list.[v1] <> [])

let AccessTo (graph : IGraph, v) =
  let arr = Array.create graph.Size false 
  arr.[v] <- true
  let rec To x =
    for i in graph.Vertices do
      if (not arr.[i]) && (graph.Edge x i) then 
        arr.[i] <- true
        To i 
  To v
  arr.[v] <- false
  let mutable ver = []
  for i in graph.Vertices do
    if arr.[i] then ver <- i :: ver
  List.rev ver  

let AccessFrom (graph : IGraph, v) =
  let arr = Array.create graph.Size false 
  arr.[v] <- true
  let rec From x =
    for i in graph.Vertices do
      if (not arr.[i]) && (graph.Edge i x) then 
        arr.[i] <- true 
        From i
  From v 
  arr.[v] <- false
  let mutable ver = []
  for i in graph.Vertices do
    if arr.[i] then ver <- i :: ver 
  List.rev ver                                                

type MarkedGr<'A> =
  inherit IGraph 
  abstract member Value : int -> int -> 'A

(*   С помощью матрицы: граф с 5 верш. и ребрами 0->1; 0->4; 1->2; 1->3; 2->3; 3->2\n   *)
let ver = [0..4]
let matr = Array2D.create 5 5 false
Array2D.set matr 0 1 true 
Array2D.set matr 0 4 true
Array2D.set matr 1 2 true
Array2D.set matr 1 3 true
Array2D.set matr 2 3 true
Array2D.set matr 3 2 true
let NewGr = new Graph (ver, matr)  

[<TestCase (0, Result = [|1; 2; 3; 4|])>]
[<TestCase (1, Result = [|2; 3|])>]
[<TestCase (2, Result = [|3|])>]
[<TestCase (3, Result = [|2|])>]
[<TestCase (4, Result = [||])>]
let accTo num = AccessTo (NewGr, num)

[<TestCase (0, Result = [||])>]
[<TestCase (1, Result = [|0|])>]
[<TestCase (2, Result = [|0; 1; 3|])>]
[<TestCase (3, Result = [|0; 1; 2|])>]
[<TestCase (4, Result = [|0|])>]
let accFrom num = AccessFrom (NewGr, num)

(*С помощью списка смежности граф из одной вершины*)
let ver1 = [0]
let list1 = []
let NewGr1 = new GraphList (ver1, list1)

[<TestCase (0, Result = [||])>]
let accTo1 num = AccessTo (NewGr1, num)

[<TestCase (0, Result = [||])>]
let accFrom1 num = AccessFrom (NewGr1, num)

(*С помощью списка смежности: граф с 5 вершинами и ребрами: 0->3 0->4 1->0 2->1 3->2*) 
let ver2 = [0..4] 
let list2 = [[3; 4]; [0]; [1]; [2]; []] 
let NewGr2 = new GraphList (ver2, list2)

[<TestCase (0, Result = [|1; 2; 3; 4|])>]
[<TestCase (1, Result = [|0; 2; 3; 4|])>]
[<TestCase (2, Result = [|0; 1; 3; 4|])>]
[<TestCase (3, Result = [|0; 1; 2; 4|])>]
[<TestCase (4, Result = [||])>]
let accTo2 num = AccessTo (NewGr2, num)

[<TestCase (0, Result = [|1; 2 ;3|])>]
[<TestCase (1, Result = [|0; 2; 3|])>]
[<TestCase (2, Result = [|0; 1; 3|])>]
[<TestCase (3, Result = [|0; 1; 2|])>]
[<TestCase (4, Result = [|0; 1; 2; 3|])>]
let accFrom2 num = AccessFrom (NewGr2, num)
 
[<EntryPoint>]
let main argv = 
    0
