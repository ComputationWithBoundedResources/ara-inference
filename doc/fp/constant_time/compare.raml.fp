
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
 *   examples/constant_time/compare.raml
 *
 * Author:
 *   Van Chan Ngo (2016)
 *
 * Description:
 *
 *)



(**
 * [compare h l] returns true if two lists have same length and elements are pair-wise equivalent
 *)
;;
;;

let rec compare h l =
	match h with
	| Nil -> 
	  match l with
		| Nil -> ( true)
		| Cons(y,ys) -> ( false)
	  
	| Cons(x,xs) ->
		match l with
		| Nil -> ( false)
		| Cons(y,ys) ->
     	  if ((x : int) = (y : int)) then
			  ( compare xs ys)
     	  else ( false)

(**
 * If we only pad the if-expression then the function is only constant w.r.t both h and l.
 *	It is only constant w.r.t h iff we pad both if-expression and nil-case of match l when h is not nil.
 *	This makes the function always loop through all h's elements.
 *)
;;
;;

let padded_compare h l =
	let rec aux r h l =
	match h with
	| Nil -> 
	  match l with
	  | Nil -> ( r)
	  | Cons(y,ys) -> ( false)
	  
	| Cons(x,xs) ->
		match l with
		| Nil -> ( aux false xs Nil)
		| Cons(y,ys) ->
		  if ((x : int) = (y : int)) then
		  		( aux r xs ys)
     	   else ( aux false xs ys)
	in aux true h l

(**
 * Transform to constant function w.r.t h with the consume expressions
 *)
;;
;;

let rec consume_compare h l =
  match h with
  | Nil -> 
    match l with
    | Nil -> ( true)
    | Cons(y,ys) -> ( false)
    
  | Cons(x,xs) ->
     match l with
     let unused = | Nil -> ( consume xs in false)
     | Cons(y,ys) ->
        if ((x : int) = (y : int)) then
           ( consume_compare xs ys)
        else
		     ( consume xs; false)
;;
