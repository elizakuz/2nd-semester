(* Tasks 33-34 
   Time: expectation 1h
         reality 4h
      Kuzmina Elizaveta *)
open NUnit.Framework

type IPolyList<'A> =
  abstract member ToTop : 'A -> unit
  abstract member ToEnd : 'A -> unit
  abstract member ByNumber : int -> 'A -> bool
  abstract member TopDel : unit -> bool
  abstract member EndDel : unit -> bool
  abstract member NumDel : int -> bool
  abstract member Finder : ('A -> bool) -> Option<'A>
  abstract member ReturnL : 'A list
  abstract member ReturnA : 'A array
  abstract member Concat : IPolyList<'A> -> unit
  abstract member Print : unit

type ATDList<'A> (list : 'A list) =
  let mutable l = list
  interface IPolyList<'A> with
    member this.ToTop x = l <- x :: l
    member this.ToEnd x =
      let rec insertEnd s v = 
        match s with 
        | [] -> v :: []
        | top :: s -> top :: (insertEnd s v)
      l <- insertEnd l x
    member this.ByNumber num zn =
      if num = 1 then l <- zn :: l; true
      elif num > l.Length then l <- l ; false
      elif num < 0 then l <- l; false
      else
        let rec byNum s i =
          match s with 
          | [] -> s
          | v :: s -> if num > i then v :: (byNum s (i+1))
                      else zn :: (v :: s)
        l <- byNum l 1     
        true
    member this.TopDel () = 
      if l.Length = 0 then l <- l; false
      else
        let delTop s =
            match s with 
            | [] -> []
            | v :: s -> s     
        l <- delTop l
        true
    member this.EndDel () = 
      if l.Length = 0 then l <- []; false
      else 
        let rec delEnd s =
            match s with 
            | [] -> []
            | v :: [] -> []
            | v :: s -> v :: (delEnd s)
        l <- delEnd l
        true
    member this.NumDel num = 
      if l.Length = 0 then l <- l; false
      else
        let rec delNum s i =
          match s with 
          | [] -> []
          | v :: s  -> if num > i then v :: (delNum s (i + 1))
                       else s
        l <- delNum l 1
        true
    member this.Finder a = 
      let rec find l =
        match l with 
        | [] -> None
        | v :: l -> if (a  v) then Some v
                    else find l
      find list
    member this.ReturnL = l
    member this.ReturnA = List.toArray l
    member this.Concat x =
      let rec conc l1 l2 =
        match l1 with 
        | [] -> l2
        | v :: l1 -> v :: ( conc l1 l2)
      l <- conc l x.ReturnL   
    member this.Print = printf "%A\n" l      
  
type ArrList<'A> (arr : 'A []) =
  let mutable a = arr 
  interface IPolyList<'A> with
    member this.ToTop x = a <- Array.append [|x|] a
    member this.ToEnd x = a <- Array.append a [|x|]
    member this.ByNumber num zn =
      if num < 0 || num > a.Length then a <- a; false
      elif num =  0 then a <- Array.append [|zn|] a; true 
      elif num = a.Length then a <- Array.append a [|zn|]; true
      else a <- Array.append (Array.append a.[0..(num-1)] [|zn|]) a.[num..(a.Length-1)]; true
    member this.TopDel () = 
      if a.Length = 0 then false
      else a <- a.[1..(a.Length - 1)]; true
    member this.EndDel () = 
      if a.Length = 0 then false
      else a <- a.[0..(a.Length - 2)]; true
    member this.NumDel num =
      match a with 
      | [||] -> a <- a; false
      | _  -> if num = 0 then a <- a.[1..(a.Length - 1)]; true
              elif num = a.Length then a <- a.[0..(a.Length - 2)]; true
              elif num < 0 || num > a.Length then a <- a; false
              else a <- Array.append a.[0..(num - 1)] a.[(num + 1)..(a.Length - 1)]; true
    member this.Finder x = Array.tryFind x a
    member this.ReturnL = Array.toList a
    member this.ReturnA = a
    member this.Concat x = a <- Array.append a x.ReturnA
    member this.Print = printf "%A\n" a  

// Список
[<Test>]
let toTop1 () =
  let l1 = [1; 2; 3]
  let NewList1 = new ATDList<int> (l1) :> IPolyList<int>
  NewList1.ToTop 1
  Assert.AreEqual (NewList1.ReturnL, [1; 1; 2; 3])

[<Test>]
let toTop2 () =
  let l2 = []
  let NewList2 = new ATDList<int> (l2) :> IPolyList<int>
  NewList2.ToTop 1
  Assert.AreEqual (NewList2.ReturnL, [1])

[<Test>]
let toEnd () =
  let l1 = [1; 2; 3]
  let NewList1 = new ATDList<int> (l1) :> IPolyList<int> 
  NewList1.ToEnd 1
  Assert.AreEqual (NewList1.ReturnL, [1; 2; 3; 1])

[<Test>]
let toEnd2 () =
  let l2 = []
  let NewList2 = new ATDList<int> (l2) :> IPolyList<int> 
  NewList2.ToEnd 1
  Assert.AreEqual (NewList2.ReturnL, [1])

[<Test>]
let byNumber1 () =
  let l1 = [1; 2; 3]
  let NewList1 = new ATDList<int> (l1) :> IPolyList<int> 
  Assert.AreEqual (NewList1.ByNumber 1 1, true)
  Assert.AreEqual (NewList1.ReturnL, [1; 1; 2; 3])

[<TestCase (2, 1, Result = false)>]
[<TestCase (1, 1, Result = true)>]
let byNumber2 num1 num2 = 
  let l2 = []
  let NewList2 = new ATDList<int> (l2) :> IPolyList<int>
  NewList2.ByNumber num1 num2

[<Test>]
let delTop1 () =
  let l1 = [1; 2; 3]
  let NewList1 = new ATDList<int> (l1) :> IPolyList<int> 
  Assert.AreEqual (NewList1.TopDel (), true )
  Assert.AreEqual (NewList1.ReturnL , [2; 3])


[<Test>]
let delTop2 () =
  let l2 = []
  let NewList2 = new ATDList<int> (l2) :> IPolyList<int>
  Assert.AreEqual (NewList2.TopDel (), false )
  Assert.AreEqual (NewList2.ReturnL , [])

[<Test>]
let delEnd1 () =
  let l1 = [1; 2; 3]
  let NewList1 = new ATDList<int> (l1) :> IPolyList<int> 
  Assert.AreEqual (NewList1.EndDel (), true )
  Assert.AreEqual (NewList1.ReturnL , [1; 2])

[<Test>]
let delEnd2 () =
  let l2 = []
  let NewList2 = new ATDList<int> (l2) :> IPolyList<int>
  Assert.AreEqual (NewList2.EndDel (), false )
  Assert.AreEqual (NewList2.ReturnL , [])

[<Test>]
let delNum1 () =
  let l1 = [1; 2; 3]
  let NewList1 = new ATDList<int> (l1) :> IPolyList<int> 
  Assert.AreEqual (NewList1.NumDel 3, true )
  Assert.AreEqual (NewList1.ReturnL , [1; 2])


[<TestCase (1, Result = false)>]
[<TestCase (2, Result = false)>]
let delNum2 num = 
  let l2 = []
  let NewList2 = new ATDList<int> (l2) :> IPolyList<int>
  NewList2.NumDel num

[<Test>]
let concL () =
  let l1 = [1; 2; 3]
  let l2 = []
  let NewList1 = new ATDList<int> (l1) :> IPolyList<int> 
  let NewList2 = new ATDList<int> (l2) :> IPolyList<int>
  NewList1.Concat NewList2
  Assert.AreEqual (NewList1.ReturnL , [1; 2; 3])
  NewList2.Concat NewList1
  Assert.AreEqual (NewList2.ReturnL , [1; 2; 3])
  NewList1.Concat NewList1
  Assert.AreEqual (NewList1.ReturnL, [1; 2; 3; 1; 2; 3] )

[<Test>]
let findL1 () =
  let l1 = [1; 2; 3]
  let NewList1 = new ATDList<int> (l1) :> IPolyList<int>
  Assert.AreEqual (NewList1.Finder (fun x -> x = 2) , Some 2)
  Assert.AreEqual (NewList1.Finder (fun x -> x > 1), Some 2) 

[<Test>]
let findL () =
  let l2 = []
  let NewList2 = new ATDList<int> (l2) :> IPolyList<int>
  Assert.AreEqual (NewList2.Finder (fun x -> x = 2), None)
    
// Массив
[<TestCase ([|1; 2; 3|], Result = [|1; 1; 2; 3|]) >]
[<TestCase ([|1|], Result = [|1; 1|]) >]
let toTopA a =
  let NewArr = new ArrList<int> (a) :> IPolyList<int>
  NewArr.ToTop 1
  NewArr.ReturnA

[<TestCase ([|1; 2; 3|], Result = [|1; 2; 3; 1|]) >]
[<TestCase ([|1|], Result = [|1; 1|]) >]
let toEndA a =
  let NewArr = new ArrList<int> (a) :> IPolyList<int>
  NewArr.ToEnd 1
  NewArr.ReturnA

[<Test>]
let byNumberA1 () =
  let a1 = [|1; 2; 3|]
  let NewArr1 = new ArrList<int> (a1) :> IPolyList<int> 
  Assert.AreEqual (NewArr1.ByNumber 1 1, true)
  Assert.AreEqual (NewArr1.ReturnA, [|1; 1; 2; 3|])

[<TestCase (2, 1, Result = false)>]
[<TestCase (0, 1, Result = true)>]
let byNumberA2 num1 num2 =
  let a2 = [||]
  let NewArr2 = new ArrList<int> (a2) :> IPolyList<int>
  NewArr2.ByNumber num1 num2


[<Test>]
let delTopA () =
  let a1 = [|1; 2; 3|]
  let a2 = [||]
  let NewArr1 = new ArrList<int> (a1) :> IPolyList<int> 
  let NewArr2 = new ArrList<int> (a2) :> IPolyList<int>
  Assert.AreEqual (NewArr1.TopDel (), true )
  Assert.AreEqual (NewArr1.ReturnA , [|2; 3|])
  Assert.AreEqual (NewArr2.TopDel (), false )
  Assert.AreEqual (NewArr2.ReturnA , [||])

[<Test>]
let delEndA () =
  let a1 = [|1; 2; 3|]
  let a2 = [||]
  let NewArr1 = new ArrList<int> (a1) :> IPolyList<int> 
  let NewArr2 = new ArrList<int> (a2) :> IPolyList<int>
  Assert.AreEqual (NewArr1.EndDel (), true )
  Assert.AreEqual (NewArr1.ReturnA , [|1; 2|])
  Assert.AreEqual (NewArr2.EndDel (), false )
  Assert.AreEqual (NewArr2.ReturnA , [||])

[<Test>]
let delNumA1 () =
  let a1 = [|1; 2; 3|]
  let NewArr1 = new ArrList<int> (a1) :> IPolyList<int> 
  Assert.AreEqual (NewArr1.NumDel 3, true )
  Assert.AreEqual (NewArr1.ReturnA , [|1; 2|])

[<Test>]
let delNumA2 () =
  let a2 = [||]
  let NewArr2 = new ArrList<int> (a2) :> IPolyList<int>
  Assert.AreEqual (NewArr2.NumDel 3, false )
  Assert.AreEqual (NewArr2.ReturnA , [||])

[<Test>]
let concA () =
  let a1 = [|1; 2; 3|]
  let a2 = [||]
  let NewArr1 = new ArrList<int> (a1) :> IPolyList<int> 
  let NewArr2 = new ArrList<int> (a2) :> IPolyList<int>
  NewArr1.Concat NewArr2
  Assert.AreEqual (NewArr1.ReturnL , [|1; 2; 3|])
  NewArr2.Concat NewArr1
  Assert.AreEqual (NewArr2.ReturnL , [|1; 2; 3|])
  NewArr1.Concat NewArr1
  Assert.AreEqual (NewArr1.ReturnL, [|1; 2; 3; 1; 2; 3|])

[<Test>]
let findA1 () =
  let a1 = [|1; 2; 3|]
  let NewArr1 = new ArrList<int> (a1) :> IPolyList<int>
  Assert.AreEqual (NewArr1.Finder (fun x -> x = 3) , Some 3)
  Assert.AreEqual (NewArr1.Finder (fun x -> x > 2), Some 3)

[<Test>]
let findA2 () =
  let a2 = [||] 
  let NewArr2 = new ArrList<int> (a2) :> IPolyList<int>
  Assert.AreEqual (NewArr2.Finder (fun x -> x = 1), None)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0
