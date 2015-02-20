type Peano = Zero | S of Peano

let suc p = 
  S p
let minus1 (p : Peano) =
  match p with
  | Zero -> Zero
  | S p -> p
let rec plus a b = 
  match a with 
  | Zero -> b 
  | S a -> S (plus a b)
let rec minus a b =
  match a, b with
  | Zero, b  -> Zero
  | a, Zero -> a
  | S a, S b -> minus a b
let rec toInt a = 
  match a with
  | Zero -> 0
  | S a -> 1 + toInt a
let rec mult a b = 
  match a, b with
  | Zero, b -> Zero
  | a, Zero -> Zero
  | x, S y -> plus (mult x y) x
let rec deg a b =
  match a, b with
  | Zero, S b -> Zero
  | a, Zero -> S (Zero) 
  | a, S b -> mult (deg a b) a   
[<EntryPoint>]
let main args =
  printf "%A\n" (toInt(plus  (S(S(S(Zero)))) (S(S(S(S(S(Zero))))))))
  printf "%A\n" (toInt(minus  (S(S(S(Zero)))) (S(S(S(S(S(Zero))))))))
  printf "%A\n" (toInt(mult  (S(S(S(Zero)))) (S(S(S(S(S(Zero))))))))
  printf "%A\n" (toInt(deg  (S(S(S(Zero)))) (S(S(S(S(S(Zero))))))))
  0
