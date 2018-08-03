
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

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Cases  * *
 *
 * File:
 *   examples/avanzini.raml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2015)
 * 
 * Description:
 *   Example from the paper "Analysing the Complexity of Functional Programs: Higher-Order Meets First-Order"
 *   by Avanzini et al. which appeared at ICFP'15.
 *   
 *)

;;

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | x::xs -> x::(append xs l2)


;;

let rec partition f l =
  match l with
    | [] -> ([],[])
    | x::xs ->
      match (partition f xs) with
        | Pair(cs,bs) -> 
      if f x then
	(cs,x::bs)
      else
	(x::cs,bs)
	

;;

let rec quicksort gt = function
  | [] -> []
  | x::xs ->
      let ys, zs = partition (gt x) xs in
      append (quicksort gt ys) (x :: (quicksort gt zs))

;;

type ('a, 'b) either = Left of 'a | Right of 'b
  
;;

let comp f x g = fun z -> f x (g z)

;;

let rec walk f xs =
  match xs with
    | [] -> (fun z ->  z)
    | x::ys -> match x with
	| Left _ -> fun y -> comp (walk f) ys (fun z -> x::z) y
	| Right l ->
	  let x' = Right (quicksort f l) in
	  fun y -> comp (walk f) ys (fun z -> x'::z) y

;;

let rev_sort f l = walk f l []

;;

let _ = rev_sort (fun a b -> a <= b) [Right [3;2;1]; Right [2;1;0;-1]; Left 1; Left 2; Left 3; Right []]


(* let rec walk xs = *)
(*   match xs with *)
(*     | [] -> (fun z ->  z) *)
(*     | x::ys -> fun y -> comp walk ys (fun z -> x::z) y ;; *)

(* let rev l = walk l [] ;; *)

