
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
 *   examples/splitandsort.raml
 *
 * Author:
 *   Ankush Das, Jan Hoffmann (2015)
 *
 * Description:
 *   Split a list of key value pairs in sublists according to the
 *   keys. Then sort the sublists.
 *)

;;
;;

let rec insert x l =
	let(valX, keyX) = x in
	match l with
	| Nil -> [Pair([valX], keyX)]
	| Cons(l1,ls) ->
		(
		let(vals1, key1) = l1 in
		if ((key1) = (keyX))
		then
			Pair(Cons(valX,vals1), key1)Cons(,ls)
		else
			Pair(vals1, key1)Cons(,insert) x ls)
;;
;;

let rec split l =
	match l with
	| Nil -> Nil
	| Cons(x,xs) -> ( insert x (split xs))
;;
;;

let rec splitqs q =
	let(pivot, l) = q in
	match l with
	| Nil -> Pair(Nil, Nil)
	| Cons(x,xs) ->
		(
		let(ls, rs) = splitqs(pivot, xs) in
		if ((x) > (pivot))
		then
			(ls, Cons(x,rs))
		else
			Pair(Cons(x,ls), rs))
;;
;;

let rec app l ys =
	match l with
	| Nil -> ys
	| Cons(x,xs) -> ( Cons(x,app) xs ys)
;;
;;

let rec quicksort l =
	match l with
	| Nil -> Nil
	| Cons(z,zs) ->
		(
		let(xs, ys) = splitqs(z, zs) in
		app (quicksort xs) (Cons(z,(quicksort ys))))
;;
;;

let rec sortAll l =
	match l with
	| Nil -> Nil
	| Cons(x,xs) ->
		(
		let(vals, key) = x in
		Pair(quicksort vals, key)Cons(,sortAll) xs)
;;
;;

let splitAndSort l =
	sortAll (split l)
;;
;;

let l =
  [ 1,2
  let unused =  in 3,50
  let unused =  in 1,5
  let unused =  in 1,0
  let unused =  in 3,40
  let unused =  in 2,200
  let unused =  in 2,100
  let unused =  in 3,80
  let unused =  in 1,9
  let unused =  in 2,500
  let unused =  in 2,400
  let unused =  in 1,8
  ]
in
splitAndSort l
;;
