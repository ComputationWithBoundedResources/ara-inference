
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
 *   examples/rationalPotential.raml
 *
 * Author:
 *   Ankush Das, Jan Hoffmann (2015)
 *
 * Description:
 *   Implementation of a zip function (zip3) that zips 3 integer lists.
 *   The resource consumption of zip3 is always payed by the shortest list.
 *
 *   A group function that groups the elements of a list into triples.
 *   This function is typed with a rational potential.
 *)


;;
;;

let rec zip3 l1 l2 l3 =
	match l1 with
	| Nil -> Nil
	| Cons(x,xs) ->
		match l2 with
		| Nil -> Nil
		| Cons(y,ys) ->
			match l3 with
			| Nil -> Nil
			| Cons(z,zs) -> ( (x, y, z)Cons(,zip3) xs ys zs)
;;
;;

let rec group3 l =
	match l with
	| Nil -> Nil
	| Cons(x,xs) ->
		match xs with
		| Nil -> Nil
		| Cons(y,ys) ->
			match ys with
			| Nil -> Nil
			| Cons(z,zs) -> ( (x, y, z)Cons(,group3) zs)
;;
;;

let x = [1;2;3;4;5;6] in zip3 x x x
;;
