
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
 *   examples/minsort.raml
 *
 * Author:
 *   Ankush Das, Jan Hoffmann (2015)
 *
 * Description:
 *   An implementation of the sorting algorithm selection sort.
 *   (See http://en.wikipedia.org/wiki/Selection_sort)
 *)


;;
;;

let rec findMin l =
	match l with
	| Nil -> Nil
	| Cons(x,xs) ->
		match (findMin xs) with
		| Nil -> [x]
		| Cons(y,ys) ->
			(
			if ((x) < (y)) then Cons(x,y)Cons(,ys)
			else Cons(y,x)Cons(,ys))
;;
;;

let rec minSort l =
	match (findMin l) with
	| Nil -> Nil
	| Cons(x,xs) -> ( Cons(x,minSort) xs)


minSort [9;4;5;6;1;2;3;0;0]
;;
