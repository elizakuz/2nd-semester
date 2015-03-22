(* Tasks 27-28 
   Time: expectation 3h
         reality 6h
      Kuzmina Elizaveta *)
type IPolyList<'A> =
  abstract member ToTop : 'A -> unit
  abstract member ToEnd : 'A -> unit
  abstract member ByNumber : int -> 'A -> unit
  abstract member TopDel : unit
  abstract member EndDel : unit
  abstract member NumDel : int -> unit
  abstract member Finder : ('A -> bool) -> Option<'A>
  abstract member ReturnL : 'A list
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
      let rec byNum s i =
        match s with 
        | [] -> failwith "Error"
        | v :: s -> if num > i then v :: (byNum s (i+1))
                    elif num = i then zn :: (v :: s)
                    else failwith "Error"
      l <- byNum l 1    
    member this.TopDel = 
      let delTop s =
        match s with 
        | [] -> failwith "Error"
        | v :: s -> s     
      l <- delTop l
    member this.EndDel = 
      let rec delEnd s =
        match s with
        | [] -> failwith "Error"
        | v :: [] -> []
        | v :: s -> v :: (delEnd s)
      l <- delEnd l
    member this.NumDel num = 
      let rec delNum s i =
        match s with 
        | [] -> failwith "Error"
        | v :: s  -> if num > i then v :: (delNum s (i + 1))
                     elif num = i then s
                     else failwith "Error"
      l <- delNum l 1  
    member this.Finder a = 
      let rec find l =
        match l with 
        | [] -> None
        | v :: l -> if (a  v) then Some v
                    else find l
      find list
    member this.ReturnL = l
    member this.Concat x =
      let rec conc l1 l2 =
        match l1 with 
        | [] -> l2
        | v :: l1 -> v :: ( conc l1 l2)
      l <- conc l x.ReturnL   
    member this.Print = printf "%A\n" l      
[<EntryPoint>]
let main args =
    let l1 = [10; 9; 1; 2; 3]
    let NewList = new ATDList<int> (l1)
    printf "Лист из 10, 9, 1, 2, 3: "
    (NewList :> IPolyList<int>).Print
    printf "Удаляем элементы из начала и конца списка: "
    (NewList :> IPolyList<int>).TopDel
    (NewList :> IPolyList<int>).EndDel
    (NewList :> IPolyList<int>).Print
    printf "Добавляем на 3 место 5: "
    (NewList :> IPolyList<int>).ByNumber 3 5
    (NewList :> IPolyList<int>).Print
    printf "Удаляем 4 элемент: "
    (NewList :> IPolyList<int>).NumDel 4
    (NewList :> IPolyList<int>).Print
    0
     
