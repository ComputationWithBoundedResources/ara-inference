
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
 *   examples/eratosthenes.raml
 *
 * Author:
 *   Ankush Das, Jan Hoffmann (2015)
 *
 * Description:
 *   Sieve of Eratosthenes.
 *)

;;
;;

let rec filter p l =
	match l with
	| Nil -> Nil
	| Cons(x,xs) ->
		(
		let xs' = filter p xs in
		if (x mod p = 0) then xs'
		else Cons(x,xs'))
;;
;;

let rec eratos l =
	match l with
	| Nil -> Nil
	| Cons(x,xs) -> ( Cons(x,(eratos (filter x xs))))

eratos [2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20]
;;