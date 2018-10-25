
let rec leqNat x y =
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
  match x with
   | 0 -> False
   | S(x') -> (match y with
              | 0 -> True
              | S(y') -> geqNat x' y')
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
let ite b th el = match b with
   | True()-> th
   | False()-> el
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
type bool = True | False
;;
type tree = Leaf of nat list
          | Node of  nat list * tree list
;;
let rec append l1 l2 =
  match l1 with
    | Nil()-> l2
    | Cons(x,xs) -> Cons(x,(append xs l2))
;;
let rec filterLeq nr xs = match xs with
  | Nil()-> Nil
  | Cons(x,xs) -> ite (leqNat nr x) (Cons(x,filterLeq nr xs)) (filterLeq nr xs)
;;
type exception =  Invalid_Tree
;;
let rec takeWhileLeq nr tree = match tree with
  | Leaf(data) -> data
  | Node(nrs,tss) ->
     (match nrs with
      | Cons(up,rest) ->
         (match rest with
          | Nil ->
             (match tss with
              | Nil()-> error Invalid_Tree
              | Cons(tLeq,lastTree) ->
                 (match lastTree with
                  | Nil()-> error Invalid_Tree
                  | Cons(tGt,empty) -> ite (leqNat nr up)
                               (append (takeWhileLeq nr tLeq) (filterLeq nr (takeWhileLeq nr tGt)))
                               (Nil)))
          | Cons(unused,unused') ->
             (match tss with
              | Nil()-> error Invalid_Tree
              | Cons(t,ts) -> ite (leqNat nr up)
                                  (append (takeWhileLeq nr t) (takeWhileLeq nr (Node(rest,ts))))
                                  (Nil)))
      | Nil()-> Nil)

;;
