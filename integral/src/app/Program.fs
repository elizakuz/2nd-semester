module integr

open calc
open System.Threading

let square (st, a, b) =
    let zn1 = Obhod(makeTree (MapIt (parser st, [("x", a)])))
    let zn2 = Obhod(makeTree (MapIt (parser st, [("x", b)])))
    let high = (zn1 + zn2) / 2.0
    if b > a then (b - a) * high 
    else (a - b) * high

let integer (x : double) : int =
    let st = x.ToString()
    let mutable res = ""
    let mutable i = 0
    while (i < st.Length) && (st.[i] <> ',') do
        res <- res + (st.[i]).ToString()
        i <- i + 1
    System.Convert.ToInt32 res

let partInt (st, a, b, stepInt : double) =
    let numOfOtr = integer ((b - a) / stepInt)
    let ost = (b - a) % stepInt
    let mutable s = 0.0
    for i = 0 to numOfOtr - 1 do
        let k = System.Convert.ToDouble (i) 
        s <- s + square (st, a + k * stepInt, a + (k + 1.0) * stepInt)
    s + square (st, b - ost, b)

let integr (st, a, b, stepInt : double, threadNumber : int) : double =
  let res = ref 0.0
  let step = (integer ((b - a) / stepInt)) / threadNumber
  let ost = b - a - System.Convert.ToDouble (step * threadNumber) * stepInt
  let threadArray = Array.init threadNumber (fun i ->
    new Thread(ThreadStart(fun _ ->
        let k = System.Convert.ToDouble (i)
        let stDoub = System.Convert.ToDouble (step)
        let mutable ind = 0.0
        if i = threadNumber - 1 then ind <- 1.0
        let a1 = a + stDoub * stepInt * k
        let b1 = a + stDoub * stepInt * (k + 1.0) + ind * ost
        lock res (fun () -> 
            res := !res + partInt (st, a1, b1, stepInt))
    ))
  ) 
  for t in threadArray do
    t.Start()
  for t in threadArray do
    t.Join()
  res.Value

let duration s f = 
  let timer = new System.Diagnostics.Stopwatch()
  timer.Start()
  let returnValue = f()
  printfn "Flows: %s\t\t\tElapsed Time: %i" s timer.ElapsedMilliseconds
  returnValue

[<EntryPoint>]
let main argv = 
    printfn "%A" (duration (sprintf "1") (fun () -> integr ("x", 0.0, 5.0, 1.0, 1))) //Elapsed Time: 58
    printfn "%A" (duration (sprintf "2") (fun () -> integr ("x", 0.0, 5.0, 1.0, 2))) //Elapsed Time: 4
    printfn "%A" (duration (sprintf "3") (fun () -> integr ("x", 0.0, 5.0, 1.0, 3))) //Elapsed Time: 5
    printfn "%A" (duration (sprintf "4") (fun () -> integr ("x", 0.0, 5.0, 1.0, 4))) //Elapsed Time: 5
    printfn "%A" (duration (sprintf "10") (fun () -> integr ("x", 0.0, 5.0, 1.0, 10))) //Elapsed Time: 12
    printfn "%A" (duration (sprintf "100") (fun () -> integr ("x", 0.0, 5.0, 1.0, 100))) //Elapsed Time: 241

    printfn "%A" (duration (sprintf "1") (fun () -> integr ("x^4 + x^3 + x^2 +x", 0.0, 5.0, 0.01, 1))) //Elapsed Time: 47
    printfn "%A" (duration (sprintf "2") (fun () -> integr ("x^4 + x^3 + x^2 +x", 0.0, 5.0, 0.01, 2))) //Elapsed Time: 44
    printfn "%A" (duration (sprintf "3") (fun () -> integr ("x^4 + x^3 + x^2 +x", 0.0, 5.0, 0.01, 3))) //Elapsed Time: 41
    printfn "%A" (duration (sprintf "4") (fun () -> integr ("x^4 + x^3 + x^2 +x", 0.0, 5.0, 0.01, 4))) //Elapsed Time: 42
    printfn "%A" (duration (sprintf "10") (fun () -> integr ("x^4 + x^3 + x^2 +x", 0.0, 5.0, 0.01, 10))) //Elapsed Time: 58
    printfn "%A" (duration (sprintf "100") (fun () -> integr ("x^4 + x^3 + x^2 +x", 0.0, 5.0, 0.01, 100))) //Elapsed Time: 274

    0
