module max

open System.Threading

let maxElem (arr : int []) l r : int =
  let mutable res = arr.[l]
  if r - l > 0 then
    for i in l+1 .. r do
      if arr.[i] > res then res <- arr.[i]
  res

let max threadNumber (arr : int []) : int =
  let res = ref 0
  let step = arr.Length / threadNumber
  let ost = arr.Length % threadNumber
  let threadArray = Array.init threadNumber (fun i ->
    new Thread(ThreadStart(fun _ ->
          let mutable ind = 0
          if i = threadNumber - 1 then ind <- 1
          let threadRes = maxElem arr (i * step) ((i+1) * step + ind * ost - 1)
          lock res (fun () ->
                      if threadRes > !res then
                        res := threadRes)
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
  let arr = Array.init 100000000 (fun i -> i)
  printfn "%A" (duration (sprintf "1") (fun () -> max 1 arr)) //Elapsed Time: 1779
  printfn "%A" (duration (sprintf "2") (fun () -> max 2 arr)) //Elapsed Time: 759
  printfn "%A" (duration (sprintf "3") (fun () -> max 3 arr)) //Elapsed Time: 836
  printfn "%A" (duration (sprintf "4") (fun () -> max 4 arr)) //Elapsed Time: 625
  printfn "%A" (duration (sprintf "5") (fun () -> max 5 arr)) //Elapsed Time: 625
  printfn "%A" (duration (sprintf "10") (fun () -> max 10 arr)) //Elapsed Time: 700
  printfn "%A" (duration (sprintf "100") (fun () -> max 100 arr)) //Elapsed Time: 971
  printfn "%A" (duration (sprintf "1000") (fun () -> max 1000 arr)) //Elapsed Time: 13343
  printfn "%A" (duration (sprintf "10000") (fun () -> max 10000 arr)) //Elapsed Time: 38753
  let arr = [|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;123;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|]
  printfn "%A" (duration (sprintf "1") (fun () -> max 1 arr)) //Elapsed Time: 26
  printfn "%A" (duration (sprintf "2") (fun () -> max 2 arr)) //Elapsed Time: 3
  printfn "%A" (duration (sprintf "3") (fun () -> max 3 arr)) //Elapsed Time: 3
  printfn "%A" (duration (sprintf "4") (fun () -> max 4 arr)) //Elapsed Time: 4
  printfn "%A" (duration (sprintf "5") (fun () -> max 5 arr)) //Elapsed Time: 6
  printfn "%A" (duration (sprintf "10") (fun () -> max 10 arr)) //Elapsed Time: 12
  printfn "%A" (duration (sprintf "100") (fun () -> max 100 arr)) //Elapsed Time: 154
  printfn "%A" (duration (sprintf "1000") (fun () -> max 1000 arr)) //Elapsed Time: 1489
  printfn "%A" (duration (sprintf "10000") (fun () -> max 10000 arr)) //Elapsed Time: 13988
  let arr = Array.init 10000 (fun i -> i)
  printfn "%A" (duration (sprintf "1") (fun () -> max 1 arr)) //Elapsed Time: 1
  printfn "%A" (duration (sprintf "2") (fun () -> max 2 arr)) //Elapsed Time: 3
  printfn "%A" (duration (sprintf "3") (fun () -> max 3 arr)) //Elapsed Time: 4
  printfn "%A" (duration (sprintf "4") (fun () -> max 4 arr)) //Elapsed Time: 5
  printfn "%A" (duration (sprintf "5") (fun () -> max 5 arr)) //Elapsed Time: 6
  printfn "%A" (duration (sprintf "10") (fun () -> max 10 arr)) //Elapsed Time: 14
  printfn "%A" (duration (sprintf "100") (fun () -> max 100 arr)) //Elapsed Time: 132
  printfn "%A" (duration (sprintf "1000") (fun () -> max 1000 arr)) //Elapsed Time: 1347
  printfn "%A" (duration (sprintf "10000") (fun () -> max 10000 arr)) //Elapsed Time: 30840
  0