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
let NewGr1 = new Graph (ver, matr)  

[<Test>]
let accTo0 () =
  Assert.AreEqual(AccessTo (NewGr1, 0), [|1; 2; 3; 4|])
  
[<Test>]
let accTo1 () =
  Assert.AreEqual(AccessTo (NewGr1, 1), [|2; 3|])

[<Test>]
let accTo2 () =
  Assert.AreEqual(AccessTo (NewGr1, 2), [|3|])

[<Test>]
let accTo3 () =
  Assert.AreEqual(AccessTo (NewGr1, 3), [|2|])

[<Test>]
let accTo4 () =
  Assert.AreEqual(AccessTo (NewGr1, 4), [||])

[<Test>]
let accFrom0 () =
  Assert.AreEqual(AccessFrom (NewGr1, 0), [||])

[<Test>]
let accFrom1 () =
  Assert.AreEqual(AccessFrom (NewGr1, 1), [|0|])

[<Test>]
let accFrom2 () =
  Assert.AreEqual(AccessFrom (NewGr1, 2), [|0; 1; 3|])

[<Test>]
let accFrom3 () =
  Assert.AreEqual(AccessFrom (NewGr1, 3), [|0; 1; 2|])

[<Test>]
let accFrom4 () =
  Assert.AreEqual(AccessFrom (NewGr1, 4), [|0|])
   
[<EntryPoint>]
let main argv = 
    0 

