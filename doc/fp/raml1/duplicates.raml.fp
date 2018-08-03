
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c
;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
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
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)

;;

let rec linear n =
  ifz n
    (fun x -> x)
    (fun n' ->

       linear n'
    )

;;

type 'a option = None | Some of 'a
;;
;;

type ('a,'b) pair = Pair of 'a * 'b
;;
;;

type 'a list = Nil | Cons of 'a * 'a list
;;
;;

type nat = 0 | S of nat
;;
;;

type Unit = Unit

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Cases  * *
 *
 * File:
 *   examples/duplicates.raml
 *
 * Author:
 *   Ankush Das, Jan Hoffmann (2015)
 *
 * Description:
 *   Remove duplicates from a list.
 *)

;;
;;

let rec eq l1 l2 =
	match l1 with
	| Nil ->
		
		match l2 with
		| Nil -> true
		| Cons(y,ys) -> false
		
	| Cons(x,xs) ->
		
		match l2 with
		| Nil -> false
		| Cons(y,ys) -> ( ((x) = (y)) && (eq xs ys))
		
;;
;;

let rec remove x l =
	match l with
	| Nil -> Nil
	| Cons(y,ys) ->
		(
		if (eq x y) then remove x ys
		else Cons(y,(remove x ys)))
;;
;;

let rec nub l =
	match l with
	| Nil -> Nil
	| Cons(x,xs) -> ( Cons(x,nub) (remove x xs))

nub [[1];[2];[3];[4];[5];[3];[1];[6];[7];[4];[1];[5];[8];[9];[8];[8];[8]]
;;
