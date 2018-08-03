
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
 *   examples/bitvectors.raml
 *
 * Author:
 *   Ankush Das, Jan Hoffmann (2015)
 *
 * Description:
 *   A library for bit vectors.
 *
 *   A bit vector is an integer list of 0's and 1's.  The first value in
 *   the list is the least significant bit.  For example [1,0] is 1 and
 *   [0,1] is 2.  We interpret the empty list as 0., i.e., Nil =
 *   [0].
 *
 *   We a assume an arbitrary but fixed word length (e.g. 32 bit) for the
 *   vectors.
 *
 *   The library contains the following functions.
 *
 *     - add(b1,b2): addition b1+b2 of two bit vectors b1 b2.  Can produce an
 *          overflow.
 *
 *     - sub(b1,b2): subtraction of b2 from b1 (b1-b2).  Returns [0,...,0] if the
 *               result is negative.
 *
 *     - compare(b1,b2): -1 if b1 is less then b2, 0 if b1 is equal to b2,
                and 1 if b1 is greater then b2.
 *
 *     - leq(b1,b2): b1 < b2 ?
 *
 *     - mult(b1,b2): multiplication of two bit vectors
 *
 *     - Div(b1,b2): division of b1 by b2.  The result is undefined in case
 *               of division by 0. (The function name is upper case
 *               since div is a RAML key word.)
 *)


;;
;;

let rec bitToInt' b n =
	match b with
	| Nil -> 0
	| Cons(x,xs) -> ( x*n + (bitToInt' xs n*n))
;;
;;

let bitToInt b =
	bitToInt' b 1
;;
;;

let sum x y r =
	(
	let s = x + y + r in
	if (s = 0) then(0, 0)
	else if (s = 1) then(1, 0)
	else if (s = 2) then(0, 1)
	else(1, 1))
;;
;;

let rec add' b1 b2 r =
	match b1 with
	| Nil -> Nil
	| Cons(x,xs) ->
		match b2 with
		| Nil -> Nil
		| Cons(y,ys) ->
			let(z, r') = sum x y r in
			( Cons(z,add') xs ys r')
;;
;;

let add b1 b2 =
	add' b1 b2 0
;;
;;

let rec mult b1 b2 =
	match b1 with
	| Nil -> Nil
	| Cons(x,xs) ->
		let zs = Cons(0,(mult xs b2)) in
		(
		if (x = 1) then add b2 zs
		else zs)
;;
;;

let diff x y r =
	( (x + y + r mod 2, if (x - y - r < 0) then 1 else 0))
;;
;;

let rec sub' b1 b2 r =
	match b1 with
	| Nil -> Pair(Nil, r)
	| Cons(x,xs) ->
		match b2 with
		| Nil -> Pair(Nil, r)
		| Cons(y,ys) ->
			let(z, r') = diff x y r in
			let(zs, s) = sub' xs ys r' in
			( if (s = 1) then Pair(Cons(0,zs), s)
			else Pair(Cons(z,zs), s))
;;
;;

let sub b1 b2 =
	( let Pair(b, _) = sub' b1 b2 0 in b)
;;
;;

let rec compare b1 b2 =
	match b1 with
	| Nil -> 0
	| Cons(x,xs) ->
		match b2 with
		| Nil -> 0
		| Cons(y,ys) ->
			let r = compare xs ys in
			(
			if (r = 0) then
				if ((x) < (y)) then 0-1
				else if ((y) < (x)) then 1
				else 0
			else r)
;;
;;

let leq b1 b2 =
	( compare b1 b2 < 1)
()
;;
