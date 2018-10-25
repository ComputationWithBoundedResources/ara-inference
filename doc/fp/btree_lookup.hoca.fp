
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
let ite2 b th el = match b with
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

type 'a treelist = NilTree | ConsTree of 'a * 'a treelist
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
          | Node of nat list * tree treelist
;;
let rec anyEq nr ys = match ys with
   | Nil()-> False
   | Cons(x,xs) -> ite (eqNat nr x) True() (anyEq nr xs)
;;
type Exception = Invalid_Tree;;

let rec lookup n node = match node with
   | Leaf(xs) -> anyEq n xs
   | Node(nrs,tss) -> match nrs with
      | Nil()-> (match tss with
                 | ConsTree(tGt,empty) -> lookup n tGt
                 | NilTree -> error)
      | Cons(nr,ns) -> match tss with
             | ConsTree(t,ts) -> ite2 (leqNat n nr) (lookup n t) (lookup n (Node(ns,ts)))
             | NilTree -> error

;;


