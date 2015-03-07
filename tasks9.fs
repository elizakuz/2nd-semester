(* Tasks 9-13
   Time: expectation 2h
         reality 2,5 h
       Kuzmina Elizaveta*)

(* Task 9. Type List.iter

val it : (('a -> unit) -> 'a list -> unit) = <fun:clo@51-3>*)

let Reverse l = List.fold ( fun y x -> x :: y) [] l
let Filter f l = Reverse (List.fold ( fun y x -> if f(x) then x :: y else y) [] l)
let Map f l = Reverse (List.fold (fun y x -> f(x) :: y) [] l)
let Horner k l = List.fold ( fun y x -> k * y + x) 0 l
[<EntryPoint>]
let main args =
  let l = [1; 2; 6; 10009; 100]
  printf "My list: %A\n" l 
  printf "Reverse l: %A\n" (Reverse l)
  printf "Filter l (x > 6): %A\n" (Filter (fun x -> x > 6) l)
  printf "Map l (x + 1): %A\n" (Map (fun x -> x + 1) l)
  printf "Gorner (substitute 2): %A\n" (Horner 2 l)
  0
