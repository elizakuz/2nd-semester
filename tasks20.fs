(* Tasks 20-25
   Time: expectation 4h
         reality 6h
      Kuzmina Elizaveta *)
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
[<EntryPoint>]
let main args =
  printf "С помощью матрицы: граф с тремя вершинфми и ребрами 0->1; 0->2; 1->2; 2->1\n"
  let ver = [0..2]
  let values = ['a'..'c']
  let matr = Array2D.create 3 3 false
  Array2D.set matr 0 1 true
  Array2D.set matr 0 2 true
  Array2D.set matr 1 2  true
  Array2D.set matr 2 1 true
  let NewGr1 = new Graph (ver, matr)
  printf "Список вершин, доступных из 0: "
  printf "%A\n" (AccessTo (NewGr1, 0)) 
  printf "Список вершин, из которых можно попасть в 2: "
  printf "%A\n" (AccessFrom (NewGr1, 2))
  let s = [[1; 2]; [2]; [1]]
  let NewGr2 = new GraphList (ver, s)
  printf "Тот же граф с помощью списка\n"
  printf "Список вершин, доступных из 2: "
  printf "%A\n" (AccessTo (NewGr2, 2))
  0
