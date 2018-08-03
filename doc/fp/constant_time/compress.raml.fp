
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
 *   examples/constant_time/compress.raml
 *
 * Author:
 *   Van Chan Ngo (2016)
 *
 * Description:
 *
 *)

(**
 * [compress l] returns a compressed list by replacing multiple sequential
 * occurences of a values by single occurrence.
 *)
;;
;;

let rec compress l =
	match l with
  	| Nil -> ( Nil)
  	| Cons(h,t) -> (
		match t with
		| Nil -> ( l)
		| Cons(x,xs) ->
			if ((x : int) = (h : int)) then
				( compress t)
			else
				( Cons(h,(compress t))))

(**
 * Transform to constant w.r.t l with the consume expressions
 *)
;;
;;

let rec consume_compress l =
	match l with
	| Nil -> ( Nil)
	| Cons(h,t) -> (
		match t with
		| Nil -> ( consume(h,t); l)
		| Cons(x,xs) ->
			if ((x : int) = (h : int)) then
				( consume(x,xs); consume_compress t)
			else
				( Cons(h,(consume_compress t))))

;;
