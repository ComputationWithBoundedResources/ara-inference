
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
 *   examples/constant_time/find.raml
 *
 * Author:
 *   Van Chan Ngo (2016)
 *
 * Description:
 *
 *)


(**
 * [find a l] returns true if there exists an element of the list [l] that is equal to a.
 * Otherwise returns false.
 *)
;;
;;

let rec find l a =
  match l with
  | Nil -> ( false)
  | Cons(h,t) ->
    if ((a : int) = (h : int)) then
	 	( true)
    else
      ( find t a)

(**
 * Manually padded function
 *)
;;

let padded_find l a =
	let rec aux res l a =
   match l with
   | Nil -> ( res)
   | Cons(h,t) ->
     if ((a : int) = (h : int)) then
 	 	( aux true t a)
     else
      ( aux res t a)
	 in aux false l a

(**
 * Transform to be constant with consume expressions.
 *)
;;
;;

let rec consume_find l a =
  match l with
  | Nil -> ( false)
  | Cons(h,t) ->
    if ((a : int) = (h : int)) then
	 	( consume(t); true)
    else
      ( consume_find t a)
;;
