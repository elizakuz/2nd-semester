(* Calculator 
   Time: expectation 5h
         reality 15h
      Kuzmina Elizaveta *)
open System.IO
open NUnit.Framework

type Token = Num of double | Op of char | LB | RB | Mut of string 

let priority op =
    match op with 
    | '^' -> 4
    | '*' | '/' | '%' -> 3
    | '+' | '-' -> 2
    | ' ' | '\n' | '\r'| '(' | ')'  -> 5
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  -> 0
    | _ -> 1

let parser (s : string) =
    let mutable list : Token list = []
    let mutable mark = false
    let mutable zn = ""
    for i = (s.Length - 1) downto 0 do 
        if priority (s.[i]) < 5 then
            if priority (s.[i]) > 1 then
                if zn <> "" then 
                    if mark then list <- (Mut zn) :: list
                    else list <- (Num (System.Convert.ToDouble (System.Convert.ToInt32(zn)))) :: list
                    zn <- ""; mark <- false
                list <- Op (s.[i]) :: list
            else zn <- s.[i].ToString () + zn
                 if priority(s.[i]) = 1 then mark <- true
        elif zn <> "" then
            if mark then list <- (Mut zn) :: list
            else list <- (Num (System.Convert.ToDouble (System.Convert.ToInt32(zn)))) :: list            
            zn <- ""; mark <-false
        if s.[i] = '(' then list <- LB :: list  
        if s.[i] = ')' then list <- RB :: list
    if zn <> "" then
        if mark then list <- (Mut zn) :: list
        else list <- (Num (System.Convert.ToDouble (System.Convert.ToInt32(zn)))) :: list
    list


type Map = MNum of float | MOp of char * int

let MapIt (list : Token list, t : list<string * double>) =
    let mutable maped : Map list = [] 
    let mutable sk : int = 0
    let mutable st : int = list.Length - 3
    for i = (list.Length - 1) downto 0 do
        match list.[i] with
        | Num x -> maped <- MNum x :: maped
        | Op x -> if x = '^' then maped <- MOp (x, (priority x) + (sk * list.Length) + st) :: maped; st <- st - 1
                  else maped <- MOp (x, (priority x) + (sk * list.Length)) :: maped
        | LB -> sk <- sk - 1
        | RB -> sk <- sk + 1
        | Mut x -> for i = 0 to t.Length - 1 do
                        match t.[i] with 
                        | (s, d) -> if s = x then maped <- MNum d :: maped
    maped
      
type Tree = Val of float | Oper of char * Tree * Tree * int | Nil

let createTArr (list : Map list) =
    let mutable t : Tree array = Array.create list.Length Nil
    for i = 0 to list.Length - 1 do
        match list.[i] with
        | MNum x -> t.[i] <- Val x
        | MOp (x, y) -> t.[i] <- Oper (x, Nil, Nil, y)
    t

let makeTree (list : Map list) =
    let mutable t = createTArr (list)
    let rec makeT (tre : Tree array) = 
        let mutable tr = tre
        let mutable st = 0
        let mutable n = 0 
        for i = 0 to (tr.Length - 1) do
            match tr.[i] with 
            | Oper (a, b, c, d) -> if d > st then st <- d; n <- i
            | Val a -> ()
            | Nil -> ()
        if n = 0 then tr.[0]
        else match tr.[n] with 
             | Nil -> Nil
             | Val a -> Val a
             | Oper (a, b, c, d) -> tr.[n - 1] <- Oper (a, tr.[n - 1], tr.[n + 1], 0)
                                    tr <- Array.append tr.[0..(n - 1)] tr.[(n + 2)..(tr.Length - 1)];
                                    makeT (tr)
    makeT (t)

let math (zn : char, v1: float, v3 : float) = 
    match zn with 
    | '^' -> v1 ** v3
    | '*' -> v1 * v3
    | '/' -> v1 / v3
    | '%' -> v1 % v3
    | '+' -> v1 + v3
    | '-' -> v1 - v3
    | _ -> 0.0

let rec Obhod (tree) =
    match tree with
    | Val x -> x
    | Nil -> 0.0
    | Oper (a, b, c, d) -> match b, c with 
                           | Val x, Val y -> math (a, x, y)
                           | Val x, _ -> math (a, x, Obhod c)
                           | _, Val x -> math (a, Obhod b, x)
                           | _, _ -> math (a, Obhod b, Obhod c)

[<TestCase ("1" , Result = 1.0)>]
[<TestCase ("1 - 2 - 3" , Result = -4.0)>]
[<TestCase ("3 ^ 1 ^ 2 ", Result = 3.0)>]
[<TestCase ("1 + 2" , Result = 3.0)>]
[<TestCase ("1 * 2" , Result = 2.0)>]
[<TestCase ("1 / 2" , Result = 0.5)>]
[<TestCase ("1 % 2" , Result = 1.0)>]
[<TestCase ("1 - 2" , Result = -1.0)>]
[<TestCase ("1 ^ 2" , Result = 1.0)>]
[<TestCase ("(10 * ( 1 + 2 * ( 3 - 2 )))" , Result = 30.0)>]
[<TestCase ("5 ^ 0 " , Result = 1.0)>]
[<TestCase ("2 ^ ( 20 - 5 * 2 )" , Result = 1024)>]
[<TestCase ("12 * ( 1 + (5 - 8 / 2 ))" , Result = 24.0)>]
let test s = Obhod (makeTree (MapIt (parser (s), [])))

//Task 36
[<TestCase ("x1", Result = 2.0)>]
[<TestCase ("1 + x2", Result = 4.0)>]
[<TestCase ("x3 ^ x1", Result = 25.0)>]
let ``Testing variables`` st = 
    Obhod( makeTree(MapIt(parser st, [("x1", 2.0); ("x2", 3.0); ("x3", 5.0) ]))) 

//task37
let lrc tr =
    let rec LRC tree : string =
        match tree with
        | Nil -> ""
        | Val x -> x.ToString() + "\n"
        | Oper (a, b, c, d) -> LRC b + LRC c + a.ToString() + "\n" 
    LRC tr


[<TestCase ("1 - 2 - 3", Result = "1\n2\n-\n3\n-\n")>]
[<TestCase ("3 ^ 1 ^ 2", Result = "3\n1\n2\n^\n^\n")>]
[<TestCase ("1 + 2 + 3", Result = "1\n2\n+\n3\n+\n")>]
[<TestCase ("1 + (2 + 3)", Result = "1\n2\n3\n+\n+\n")>]
[<TestCase ("1 * (3 - 2)", Result = "1\n3\n2\n-\n*\n")>]
let r s = lrc(makeTree (MapIt (parser (s), [])))

let writePolZap (fileFrom : string, fileIn : string) =
    use sRead = new StreamReader(fileFrom)
    let mutable s = sRead.ReadToEnd ()
    let mutable t = makeTree (MapIt (parser (s), []))
    use sWrite = new StreamWriter (fileIn)
    sWrite.WriteLine (lrc t)

//task38
let rec makeT (tre : Tree array) = 
        let mutable tr = tre
        let mutable st = true
        let mutable n = 0 
        for i = 0 to (tr.Length - 1) do
            match tr.[i] with 
            | Oper (a, b, c, d) -> if d > 0 && st then n <- i; st <- false
            | Val a -> ()
            | Nil -> ()
        if tr.Length = 1 then tr.[0]
        else match tr.[n] with 
             | Nil -> Nil
             | Val a -> Val a
             | Oper (a, b, c, d) -> tr.[n - 2] <- Oper (a, tr.[n - 2], tr.[n - 1], 0)
                                    tr <- Array.append tr.[0..(n - 2)] tr.[(n + 1)..(tr.Length - 1)];
                                    makeT (tr)

[<TestCase ("1", Result = 1.0)>]
[<TestCase ("1 \n 2 \n -", Result = -1.0)>]
[<TestCase ("7 \n 6 \n 5 \n + \n * ", Result = 77.0)>]
[<TestCase ("2 \n 4 \n ^", Result = 16.0)>]
[<TestCase ("1 \n 1 \n - \n 1 \n -", Result = -1.0)>]
[<TestCase ("9 \n 2 \n % \n 9 \n * \n 3 \n 3 \n * \n *", Result = 81.0)>]
let test2 s = Obhod (makeT (createTArr (MapIt (parser (s), []))))

let writeAnswer () =
    use sRead = new StreamReader("MyTest2.txt")
    let mutable s = sRead.ReadToEnd ()
    let mutable list = parser (s)
    let mutable map = MapIt (list, []) 
    let mutable t = createTArr (map)
    use sWrite = new StreamWriter ("a2.test")
    sWrite.WriteLine (Obhod (makeT t))

[<EntryPoint>]
let main args=
    0
