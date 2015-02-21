type Tree = Nil | T of Tree * int * Tree

let rec insert x t =
  match t with
  | Nil -> T (Nil, x, Nil)
  | T (L, a, R) -> if x < a then T (insert x L, a, R) 
                   else if x = a then T (L,a , R)
                   else T (L, a, insert x R)
let rec bigInt t =
  match t with
  | Nil -> 0
  | T (L, a, R) -> if R = Nil then a
                   else bigInt R                    
let rec delete x t =
  match t with 
  | Nil -> Nil
  | T (L, a, R) -> if x < a then T (delete x L, a, R)
                   else if x > a then T (L, a, delete x R)
                        else match L, R with
                             | Nil, Nil -> Nil
                             | Nil, T1-> T1 
                             | T1, Nil -> T1
                             | T1, T (l, b, r) -> if l = Nil then T (T1, b, r)
                                                   else T (delete (bigInt T1) T1, bigInt T1, R)                                                      
let rec printLCR t =
  match t with
  | Nil -> printf ""
  | T (L, a, R) -> printLCR L
                   printf "%d " a
                   printLCR R 
let rec printLRC t =
  match t with
  | Nil -> printf ""
  | T (L, a, R) -> printLRC L
                   printLRC R
                   printf "%d " a 
let rec printCLR t =
  match t with
  | Nil -> printf ""
  | T (L, a, R) -> printf "%d " a 
                   printCLR L
                   printCLR R
[<EntryPoint>]
let main args =
  let  t = insert 1 Nil
  let t = insert 5 t
  let t = insert 10 t
  let t = insert 9 t
  let t = insert 3 t
  let t = insert 2 t 
  let t = insert 4 t
  let t = delete 5 t
  printLCR t
  printf "\n"
  printLRC t
  printf "\n"
  printCLR t
  0
