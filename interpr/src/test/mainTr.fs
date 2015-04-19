module mainTr
open parser
open exTree

type tree =
       | Null
       | Read   of string
       | Write  of exTree
       | Assign of string * exTree
       | Seq    of tree * tree
       | If     of exTree * tree * tree
       | While  of exTree * tree

let delExpr (arr : t array) =
    let mutable l = group arr
    let mutable list = []
    for i = 0 to arr.Length - 1 do 
        match arr.[i] with 
        | Command y -> match y with 
                       | "read" -> list <- Read l.Head.Head :: list; l <- l.Tail
                       | "write" -> list <- Write (makeExTree (exList l.Head)) :: list;
                                    l <- l.Tail
                       | ":=" -> list <- Assign (l.Head.Head, makeExTree(exList l.Head.Tail)) :: list;
                                 l <- l.Tail
                       | ";" -> list <- Seq (Null, Null) :: list
                       | "if" -> list <- If (makeExTree (exList l.Head), Null, Null) :: list;
                                 l <- l.Tail
                       | "while" -> list <- While (makeExTree (exList l.Head), Null) :: list;
                                    l <- l.Tail
                       | _ -> ()
        | _ -> ()
    List.toArray (List.rev list)

let toTree array =
  let rec makeTree (arr : tree array) (l : int array) =
    match arr.[0] with
    | Seq (a, b) -> let r = l.[0]
                    l.[0] <- (l.[0] + 1)
                    let t1 = makeTree arr.[1..arr.Length - 1] l
                    let t2 = makeTree arr.[l.[0] - r + 1..arr.Length - 1] l
                    match t1, t2 with
                    | (v1, _), (v2, _) -> arr.[0] <- Seq (v1, v2)
                     
    | If (a, b, c) -> let r = l.[0]
                      l.[0] <- (l.[0] + 1) 
                      let t1 = makeTree arr.[1..arr.Length - 1] l
                      let t2 = makeTree arr.[l.[0] - r + 1..arr.Length - 1] l
                      match t1, t2 with
                      | (v1, _), (v2, _) -> arr.[0] <- If (a, v1, v2)
                     
    | While (a, b) -> l.[0] <- (l.[0] + 2) 
                      let t1 = makeTree arr.[1..arr.Length - 1] l
                      match t1 with
                      | (v1, _) -> arr.[0] <- While (a, v1)
    | _ -> ()
    (arr.[0], l)
  match makeTree array [|0|] with
  | (a, _) -> a                    


