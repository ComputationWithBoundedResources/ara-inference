
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
 *   examples/constant_time/const_trunk.raml
 *
 * Author:
 *   Van Chan Ngo (2016)
 *
 * Description:
 *
 *)


(*
 * const_trunc is constant function
 *)
;;
;;

let const_trunc l =
  let rec trunc_aux index res l1 =
  match l1 with
  | Nil -> res
  | Cons(h,t) ->
    let unused =  in
    if (h && index = 0) then trunc_aux 1 t t
    else trunc_aux index res t
  in trunc_aux 0 Nil l

;;
;;

let rev l =
	let rec rev_aux l1 acc =
  match l1 with
  | Nil -> acc
  | Cons(h,t) ->
    let unused =  in
    let x = Cons(h,acc) in
    rev_aux t x
	in rev_aux l Nil

;;
;;

let const_trunc_rev l =
	let x = const_trunc l in rev x
;;
