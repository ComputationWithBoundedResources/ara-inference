
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
 *   examples/subsequence.raml
 *
 * Author:
 *   Jan Hoffmann (2015)
 * 
 * Description:
 *   A standard example of dynamic programming that can be found in
 *   many textbooks (see e.g. Cormen/Leiserson/Rivest/Stein:
 *   Introduction to Algorithms) is the computation of the length of
 *   the longest common subsequence (LCS) of two given lists
 *   (sequences).  Given two sequences a_1,...,a_n and b_1,...,b_m, one
 *   successively fills an nxm-matrix (here a list of lists) A such
 *   that A(i,j) contains the length of the LCS of a_1,...,a_i and
 *   b_1,...,b_j.  It is the case that
 *
 *            { 0                      if i=0 or j=0
 *   A(i,j) = { A(i-1,j-1) + 1         if i,j > 0 and a_i = b_j 
 *            { max(A(i,j-1),A(i-1,j)) if i,j > 0 and a_i \= b_j
 *
 *   This algorithm can be analyzed in our system and is exemplary for
 *   similar algorithms that use dynamic programming.
 *)




(* Returns the first line of 0s *)
;;

let rec firstline l =
  match l with 
    | [] -> []
    | x::xs -> 0::(firstline xs)


(* computes a new line according to the recursive definition above
 * y is the element of the second list
 * lastline the the previously computed line in the matrix
 * l contains elements of the first list *)
;;

let rec newline y lastline l =
  let max a b = if (a:int)>b then a else b in
  let head_or_0 l =
    match l with
      | [] -> 0
      | x::xs -> x
  in
  match l with
    | []     -> []
    | (x::xs) ->
      match lastline with
        | [] -> []
        | belowVal::lastline' ->
	  let nl = newline y lastline' xs in
          let rightVal = head_or_0 nl in
          let diagVal =  head_or_0 lastline' in
          let elem =
	    if (x:int) = y then
	      diagVal+1
	    else
	      max belowVal rightVal
          in
	  elem::nl


(* computes the table of lengths *)
;;

let rec lcstable l1 l2 =
  match l1 with
    | [] -> [firstline l2]
    | x::xs ->
      let m = lcstable xs l2 in
      match m with
        | [] -> []
        | l::ls -> (newline x l l2)::l::ls


(* computes the length of the longest common subsequence *)
;;

let rec lcs l1 l2 =
  let m = lcstable l1 l2 in
  match m with
    | [] -> 0
    | l1::_ ->
      match l1 with
        | [] -> 0
        | len::_ -> len

;;

let l1 = [1;2;3;4;5;6;7;8;9;10]
;;

let l2 = [5;3;2;6;7;1;8;0;9;11;101;102;1;2;3;10]
  
;;

let _ = lcs l1 l2


