module parser

type t =
       | Expr of string
       | Command of string

let parser (str : string) = 
    let mutable l : t list = []
    let mutable s = str
    let mutable wrem = ""
    while s.Length > 0 do
        match s.[0] with 
        | ' ' | '\n' | '\r' -> if s.Length > 1 then s <- s.[1..s.Length - 1]
        | _ -> while s.[0] <> ' ' && s.[0] <> '\n' && s.[0] <> '\r' && s.Length > 1 do
                    wrem <- wrem + s.[0].ToString()
                    s <- s.[1..s.Length - 1]
               if s.[0] <> ' ' && s.[0] <> '\n' && s.[0] <> '\r' && s.Length = 1 then
                    wrem <- wrem + s.[0].ToString() 
                    s <- ""
               if s.Length = 1 then 
                    s <- ""
               match wrem with 
               | "read" | "write" | ":=" | ";" | "if" | "while" -> l <- Command wrem :: l
               | _ -> l <- Expr wrem :: l
               wrem <- "" 
    List.toArray (List.rev l)

let size (a : t []) =
    let mutable num = 0
    for i = 0 to a.Length - 1 do
        match a.[i] with
        | Command x -> num <- num + 1
        | _ -> ()
    num

let group arr =
    let mutable list = []
    let mutable a = arr
    while a <> [||] do
        match a.[0] with
        | Expr x -> let mutable otr = [x]
                    if a.Length > 1 then do
                      let mutable i = 1
                      while i < a.Length do
                          match a.[i] with
                          | Expr y -> otr <- y :: otr;
                                      i <- i + 1;
                                      if i = a.Length then a <- [||]
                          | _ -> a <- a.[i..a.Length - 1]
                                 i <- a.Length
                    else a <- [||]
                    list <- (List.rev otr) :: list
        | _ -> if a.Length > 1 then a <- a.[1..a.Length - 1]
               else a <- [||]
    List.rev list