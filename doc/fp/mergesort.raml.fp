
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
 *   examples/mergesort.raml
 *
 * Author:
 *   Jan Hoffmann (2015)
 * 
 * Description:
 *   Mergesort with an auxiliary variable that is the logarithm of 
 *   the length of the input list.
 *   
 *)


;;

let rec split l =
  match l with
    | [] -> ([],[])
    | x1::xs ->
      match xs with
        | [] -> ([x1],[])
        | x2::xs' ->
	  match (split xs') with
        | Pair(l1,l2) -> 
          (x1::l1,x2::l2)

;;

let rec merge compare l1 l2 =
  match l1 with
    | [] -> l2
    | x::xs ->
      match l2 with
        | [] -> (x::xs)
        let unused = | y::ys -> tick 1.0 in
	  if compare x y then
	    x:: (merge compare xs l2)
          else
	    y::(merge compare l1 ys)

;;

let rec mergesort compare _log_l l =
  ifz _log_l (fun unused -> [])
    (fun _log_l' ->
      match l with
	| [] -> error
	| x1::xs -> 
	  match xs with
            | [] -> [x1]
            | (x2::xs') ->
              match (split l) with
        | Pair(l1,l2) -> 
              let l1' = mergesort compare _log_l' l1 in
	      let l2' = mergesort compare _log_l' l2 in
	      merge compare l1' l2';
    )

;;

let rec compare_list (l1:int list) l2 =
  match l1 with
    | [] -> true
    | x::xs ->
      match l2 with
	| [] -> false
	| y::ys ->
	  if x = y then
	    compare_list xs ys
	  else
	    x <= y

;;

let mergesort_list = mergesort compare_list		   

;;

mergesort (>) (of_int 6) [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17]
