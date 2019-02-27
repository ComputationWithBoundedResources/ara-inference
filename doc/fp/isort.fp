
let rec leqNat y x =
  match y with
  | 0 -> True
  | S(y') -> (match x with
            | S(x') -> leqNat x' y'
            | 0 -> False)
;;
let rec eqNat x y =
  match y with
  | 0 -> (match x with
      | 0 -> True
      | S(x') -> False)
  | S(y') -> (match x with
            | S(x') -> eqNat x' y'
            | 0 -> False)
;;
let rec geqNat x y =
  match y with
  | 0 -> True
  | S(y') -> (match x with
             | 0 -> False
             | S(x') -> geqNat x' y')
;;
let rec ltNat x y =
  match y with
   | 0 -> False
   | S(y') -> (match x with
        | 0 -> True
        | S(x') -> ltNat x' y')
;;
let rec gtNat x y =
  match x with
   | 0 -> False
   | S(x') -> (match y with
             | 0 -> True
             | S(y') -> gtNat x' y')


;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
;;
let ite cond thenPart elsePart = match cond with
   | True -> thenPart
   | False -> elsePart
;;
let ite2 cond thenPart elsePart = match cond with
   | True -> thenPart
   | False -> elsePart
;;
let minus n m =
  let rec minus' m n = match m with
        | 0 -> 0
        | S(x) -> (match n with
          | 0 -> m
          | S(y) -> minus' x y)
  in Pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c
;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> (match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> (match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)))
;;
let rec mult n m = match n with
   | 0 -> 0
   | S(x) -> S(plus (mult x m) m)
;;
type bool = True | False
;;
type 'a option = None | Some of 'a
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type ('a,'b) pair = Pair of 'a * 'b


;;
let rec compare_list l1 l2 =
  match l1 with
  | Nil()-> True
  | Cons(x,xs) ->
     (match l2 with
      | Nil()-> False
      | Cons(y,ys) -> ite2 (eqNat x y) (compare_list xs ys) (ltNat x y))

;;
let rec insert le x l =
  match l with
  | Nil()-> Cons(x,Nil)
  | Cons(y,ys) -> ite (le y x) Cons(y,insert le x ys) Cons(x,Cons(y,ys))

;;
let rec isort le l =
  match l with
  | Nil()-> Nil
  | Cons(x,xs) -> insert le x (isort le xs)
;;
let isort_list = isort compare_list
;;

let main xs = isort_list xs
;;
