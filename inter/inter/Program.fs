type Expr =
    | Nil
    | Val of string
    | Oper of char * Expr * Expr

let rec makeT (tre : Expr array) = 
        let mutable tr = tre
        let mutable n = 0 
        for i = 0 to (tr.Length - 1) do
            match tr.[i] with 
            | Oper (a, b, c) -> n <- i
            | Val a -> ()
            | Nil -> ()
        if tr.Length = 1 then tr.[0]
        else match tr.[n] with 
             | Nil -> Nil
             | Val a -> Val a
             | Oper (a, b, c) -> tr.[n] <- Oper (a, tr.[n + 1], tr.[n + 2])
                                 tr <- Array.append tr.[0..(n)] tr.[(n + 3)..(tr.Length - 1)];
                                 makeT (tr)
type t = 
       | Not
       | Ex of Expr
       | Read   of t
       | Write  of t
       | Assign of t * t
       | Seq    of t * t
       | If     of t * t * t
       | While  of t * t

let parser str =
    let mutable l : t list = []
    let mutable s = str
    let mutable wrem = ""
    while s <> "" do
        match s.[0] with 
        | ' ' | '\n' | '\r' -> s <- s.[1..s.Length - 1]
        | _ -> while s.[0] <> ' ' do 
                   wrem <- wrem + s.[0].ToString()
                   s <- s.[1..s.Length - 1]
               match wrem with 
               | "Read" -> l <- Read Not :: l
               | "Write" -> l <- Write Not :: l
               | ":=" -> l <- Assign (Not, Not) :: l
               | ";" -> l <- Seq (Not, Not) :: l
               | "If" -> l <- If (Not, Not, Not) :: l
               | "While" -> l <- While (Not, Not) :: l
               | _ -> l <- Ex (Val wrem) :: l
               wrem <- ""
    List.toArray l
    
let group arr =
    let mutable a = arr
    let rec gr a : Expr list =
        match a.[0] with 
        | Ex x -> gr a.[1..a.Length - 1]; x :: l
        | _ -> ()


let findSeq (a : t array) : int = 
    let mutable num = 0
    for i = (a.Length - 1) downto 1 do
        match a.[i] with
        | Seq _ -> num <- i
        | _ -> num <- 0
    num

let Tree (a : t array) =
    let mutable ch = 0
    let rec treeIt (arr : t array, ch) : int * t =
        let mutable m = arr
        match arr.[0] with
        | Read _ ->  (ch + 1, arr.[1])
        | Write _ -> (ch + 1, Write arr.[1])
        | Assign _ -> (ch + 1, Assign (arr.[1], treeIt (arr.[2..arr.Length - 1], ch)))
        | Seq _ -> (ch + 1,
                   if findSeq arr <> 0 then Seq (treeIt arr.[1..arr.Length - 1],
                                                 treeIt arr.[findSeq arr..arr.Length - 1], ch)
                   else Seq (treeIt arr.[1..arr.Length - 1], ch, Not))
        | If _ -> (ch + 1,
                  If (treeIt arr.[1..arr.Length - 1],
                                       treeIt arr.[findSeq arr..arr.Length - 1],
                                       treeIt arr.[findSeq arr.[findSeq arr..arr.Length - 1]..arr.Length - 1]))
        | While _ -> (ch + 1,
                     While (treeIt arr.[1..arr.Length - 1],
                                       treeIt arr.[findSeq arr..arr.Length - 1]))
        | Not -> ch <- ch + 1; Not
        //| Ex x -> ch <- ch + 1;
          //        match x with 
            //      | Nil -> Ex x
              //    | Val y -> Ex x
                //  | Oper (a, b, c) -> Ex Oper (a, treeIt arr.[1..arr.Length - 1], treeIt arr.[1..arr.Length - 1])
        
    treeIt (Array.append [|treeIt a|] a.[ch..a.Length - 1]) 
    
              


        

                    


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

