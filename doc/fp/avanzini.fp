
let rec leq x y =
  match y with
  | 0 -> True
  | S(y') -> match x with
            | S(x') -> leq x' y'
            | 0 -> False
;;
let req eq x y =
  match y with
  | 0 -> match x with
      | 0 -> True
      | S(x') -> False
  | S(y') -> match x with
            | S(x') -> eq x' y'
            | 0 -> False
;;
let rec geq x y =
  match x with
   | 0 -> False
   | S(x') -> match y with
              | 0 -> True
              | S(y') -> geq x' y'
;;
let rec lt x y =
  match y with
   | 0 -> False
   | S(y') -> match x with
        | 0 -> True
        | S(x') -> lt x' y'
;;
let rec gt x y =
  match x with
   | 0 -> False
   | S(x') -> match y with
             | 0 -> True
             | S(y') -> gt x' y'

;;
type bool = True | False
;;
type 'a option = None | Some of 'a
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type nat = 0 | S of nat
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type Unit = Unit
;;
type ('a, 'b) either = Left of 'a | Right of 'b

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
        | S(x) -> match n with
          | 0 -> m
          | S(y) -> minus' x y
  in Pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c

;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Cases  * *
 *
 * File:
 *   examples/avanzini.raml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (S(S(0))015)
 *
 * Description:
 *   Example from the paper "Analysing the Complexity of Functional Programs: Higher-Order Meets First-Order"
 *   by Avanzini et al. which appeared at ICFP'15.
 *
 *)
;;
let rec append l1 l2 =
  match l1 with
    | Nil()-> l2
    | Cons(x,xs) -> Cons(x,(append xs l2))

;;
let rec partition f l =
  match l with
    | Nil()-> Pair(Nil,Nil)
    | Cons(x,xs) ->
      match (partition f xs) with
        | Pair(cs,bs) -> ite (f x) Pair(cs,Cons(x,bs)) Pair(Cons(x,cs),bs)

;;
let rec quicksort gt xxs = match xxs with
  | Nil()-> Nil
  | Cons(x,xs) ->
      match (partition (fun y -> gt x y) xs) with
      | Pair(ys,zs) -> append (quicksort gt ys) (Cons(x,(quicksort gt zs)))
;;
let comp f x g = fun z -> f x (g z)
;;
let rec walk f xs =
  match xs with
    | Nil()-> (fun z ->  z)
    | Cons(x,ys) -> match x with
	| Left(unused) -> fun y -> comp (walk f) ys (fun z -> Cons(x,z)) y
	| Right(l) ->
	  let x' = Right (quicksort f l) in
	  fun y -> comp (walk f) ys (fun z -> Cons(x',z)) y
;;
let rev_sort f l = walk f l Nil
;;
let main xs = rev_sort (fun a b -> leq a b) xs


(* let rec walk xs = *)
(*   match xs with *)
(*     | Nil -> (fun z ->  z) *)
(*     | Cons(x,ys) -> fun y -> comp walk ys (fun z -> Cons(x,z)) y  *)

(* let rev l = walk l Nil  *)

;;
