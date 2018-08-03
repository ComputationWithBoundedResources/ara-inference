
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
 *   examples/constant_time/filter.raml
 *
 * Author:
 *   Van Chan Ngo (2016)
 *
 * Description:
 *
 *)


(**
 * [filter l] returns a list by removing all positive elements
 * and increases other elements by 1.
 * Filtering out an element takes 8 resource units.
 * Increasing an element takes 3 units.
 *)

;;
;;

let rec filter l =
 match l with
 | Nil -> ( Nil)
 | Cons(x,xs) ->
   if x > 0 then
     ( filter xs)
   else
     ( (x+1)Cons(,filter) xs)
;;
;;

let f l = filter (filter l)

(**
 * Transform to constant function w.r.t the argument l with consume expression
 *)
;;

let rec consume_filter l =
 match l with
 | Nil -> ( Nil)
 | Cons(x,xs) ->
   if x > 0 then
     ( consume_filter xs)
   else
     let unused = ( consume(x,xs) in (x+1)Cons(,consume)_filter xs)
;;
