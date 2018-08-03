
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

type Unit = Unit

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Cases  * *
 *
 * File:
 *   examples/raml1_mergesort.raml
 *
 * Author:
 *   Ankush Das, Jan Hoffmann (2015)
 *
 * Description:
 *   An implementation of the well known sorting algorithm mergesort.
 *   This implementation deallocates the input list.  Copy the list before
 *   if you want to use the unsorted list later.
 *
 *   It is suprising that the function mergesort can be analysed.  The
 *   type of the function looks like the result of a bug on the first
 *   view.  But it is correct.  The function mergesort is a nice
 *   example of a type that might be hard to find without type
 *   inference.
 *
 *   There is also an out commented buggy version of mergesort
 *   (mergesortBuggy) that does not terminate and consumes an infinite
 *   ammount of heap-space and time.  Thus our analysis does not work
 *   for mergesortBuggy.  (Note that it is no general problem for our
 *   analysis if a function does not terminate.  For example f(x) =
 *   f(x) consumes no heap-space and can be analysed with the
 *   heap-space metric.)
 *)

;;
;;

let rec msplit l =
	match l with
	| Nil -> Pair(Nil, Nil)
	| Cons(x1,xs) ->
		match xs with
		| Nil -> Pair([x1], Nil)
		| Cons(x2,xs') ->
			(
			let(l1, l2) = msplit xs' in
			(Cons(x1,l1), Cons(x2,l2)))
;;
;;

let rec merge l1 l2 =
	match l1 with
	| Nil -> l2
	| Cons(x,xs) ->
		match l2 with
		| Nil -> Cons(x,xs)
		| Cons(y,ys) ->
			(
			if ((x) < (y)) then Cons(x,merge) xs (Cons(y,ys))
			else Cons(y,merge) (Cons(x,xs)) ys)
;;
;;

let rec mergesortBuggy l =
	match l with
	| Nil -> Nil
	| Cons(x1,xs) ->
		let(l1, l2) = msplit l in
		( merge (mergesortBuggy l1) (mergesortBuggy l2))
;;
;;

let rec mergesort l =
	match l with
	| Nil -> Nil
	| Cons(x1,xs) ->
		match xs with
		| Nil -> Cons(x1,xs)
		| Cons(x2,xs') ->
			let(l1, l2) = msplit (Cons(x1,x2)Cons(,xs')) in
			( merge (mergesort l1) (mergesort l2))

mergesort [0;4;5;9;7;1;2;8;6;3]
;;
