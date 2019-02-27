
let rec leqNat y x =
  match y with
  | 0 -> True
  | S(y') -> (match x with
            | S(x') -> leqNat x' y'
            | 0 -> False)
;;
let rec eqNat x y =
  match y with
  | 0 -> (match x with
      | 0 -> True
      | S(x') -> False)
  | S(y') -> (match x with
            | S(x') -> eqNat x' y'
            | 0 -> False)
;;
let rec geqNat x y =
  match y with
  | 0 -> True
  | S(y') -> (match x with
             | 0 -> False
             | S(x') -> geqNat x' y')
;;
let rec ltNat x y =
  match y with
   | 0 -> False
   | S(y') -> (match x with
        | 0 -> True
        | S(x') -> ltNat x' y')
;;
let rec gtNat x y =
  match x with
   | 0 -> False
   | S(x') -> (match y with
             | 0 -> True
             | S(y') -> gtNat x' y')


;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
;;
let ite b th el = match b with
   | True()-> th
   | False()-> el
;;
let minus n m =
  let rec minus' m n = match m with
        | 0 -> 0
        | S(x) -> (match n with
          | 0 -> m
          | S(y) -> minus' x y)
  in Pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c
;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> (match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> (match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)))
;;
let rec mult n m = match n with
   | 0 -> 0
   | S(x) -> S(plus (mult x m) m)
;;
type bool = True | False
;;
type 'a option = None | Some of 'a
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type ('a,'b) pair = Pair of 'a * 'b

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
 *   Jan Hoffmann (S(S(0))015)
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
 *            { 0                      ite (i=0 or j=0)
 *   A(i,j) = { A(iS(0),jS(0)) + S(0)         ite (i,j > 0 and a_i = b_j)
 *            { max(A(i,jS(0)),A(iS(0),j)) ite (i,j > 0 and a_i \= b_j)
 *
 *   This algorithm can be analyzed in our system and is exemplary for
 *   similar algorithms that use dynamic programming.
 *)


;;
(* Returns the first line of zeros *)
let rec firstline l =
  match l with
    | Nil()-> Nil
    | Cons(x,xs) -> Cons(0,(firstline xs))

;;
(* computes a new line according to the recursive definition above
 * y is the element of the second list
 * lastline the the previously computed line in the matrix
 * l contains elements of the first list *)
let rec newline y lastline l =
  let max a b = ite (gtNat a b) a b in
  let head_or_zero l =
    match l with
      | Nil()-> 0
      | Cons(x,xs) -> x
  in
  match l with
    | Nil()    -> Nil
    | Cons(x,xs) ->
      (match lastline with
        | Nil()-> Nil
        | Cons(belowVal,lastline') ->
	         let nl = newline y lastline' xs in
           let rightVal = head_or_zero nl in
           let diagVal =  head_or_zero lastline' in
           let elem = ite (eqNat x y) S(diagVal) (max belowVal rightVal)
           in Cons(elem,nl))

;;
(* computes the table of lengths *)
let rec lcstable l1 l2 =
  match l1 with
    | Nil()-> (Cons(firstline l2,Nil))
    | Cons(x,xs) ->
      let m = lcstable xs l2 in
      (match m with
       | Nil() -> Nil
       | Cons(l,ls) -> Cons(newline x l l2,Cons(l,ls)))
;;

(* computes the length of the longest common subsequence *)
let rec lcs l1 l2 =
  let m = lcstable l1 l2 in
  match m with
    | Nil()-> 0
    | Cons(l1,unused) ->
      (match l1 with
        | Nil()-> 0
        | Cons(len,unused) -> len)
;;
let main l1 l2 = lcs l1 l2
;;
