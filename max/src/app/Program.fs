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
  let threadArray = Array.init threadNumber (fun i ->
      new Thread(ThreadStart(fun _ ->
          let threadRes = maxElem arr (i * step) ((i+1) * step - 1)
          if threadRes > res.Value then res := threadRes
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
    let a = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 123; 0; 1|]
    printfn "%A" (duration (sprintf "1") (fun () -> max 1 a)) //Elapsed Time: 33
    printfn "%A" (duration (sprintf "2") (fun () -> max 2 a)) //Elapsed Time: 4
    printfn "%A" (duration (sprintf "3") (fun () -> max 3 a)) //Elapsed Time: 6
    printfn "%A" (duration (sprintf "4") (fun () -> max 1 a)) //Elapsed Time: 1
    printfn "%A" (duration (sprintf "5") (fun () -> max 2 a)) //Elapsed Time: 2
    printfn "%A" (duration (sprintf "100") (fun () -> max 3 a)) //Elapsed Time: 11 
    0
