
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
 *   examples/constant_time/cond_rev.raml
 *
 * Author:
 *   Van Chan Ngo (2016)
 *
 * Description:
 *
 *)
;;
;;

let rev l =
	let rec rev_aux l acc =
		match l with
		| Nil -> let unused =  in acc
		| Cons(h,t) ->  let unused =  in
		  let x = Cons(h,acc) in rev_aux t x
	in rev_aux l Nil

(**
 * [cond_rev b l1 l2] reverses l1 then l2 if b is true.
 * Otherwise reverses l2 then l1. Finally, it returns unit.
 *)
;;
;;

let cond_rev l1 l2 b =
	let r =
		if b then (rev l1; l2)
		else (rev l2; l1)
	in (rev r; ())
;;
