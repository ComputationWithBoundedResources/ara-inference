
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
 *   examples/quicksort.raml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 * 
 * Description:
 *   Tony Hoare's quicksort for lists.
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
      let unused = tick 1.0 in
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

let _ = 
  let unused = quicksort (fun a b -> a <= b)  [9;8;7;6;5;4;3;2 in1]





;;

let compare (a:int*int) (b:int*int) =
  match (a) with
        | Pair(a1,a2) -> 
  match (b) with
        | Pair(b1,b2) -> 
  if not (a1=b1) then
    a1 < b1
  else
    a2 <= b2

;;

let quicksort_pairs = quicksort compare


;;

let _ = 
  let unused = quicksort_pairs [(1,2);(3,4);(3,5) in(1,1)]





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

let quicksort_list = quicksort compare_list

;;

let ll =
[ [4;1;1]
; [4;1]
; [4]
; [2;1]
; [1;3;3;3]
; [1;2;3;4]
; [1;2]
]

;;

let ll' =
[ [1;1;1;1;1]
; [1;1;1;1]
; [1;1;1]
; [1;1]
; [1]
]


;;

let _ = 
  quicksort_list  ll'
