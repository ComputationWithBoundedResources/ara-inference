let rec leqNat x y =
  match y with
  | 0 -> True
  | S(y') -> match x with
            | S(x') -> leqNat x' y'
            | 0 -> False
;;
let rec eqNat x y =
  match y with
  | 0 -> match x with
      | 0 -> True
      | S(x') -> False
  | S(y') -> match x with
            | S(x') -> eqNat x' y'
            | 0 -> False
;;
let rec geqNat x y =
  match x with
   | 0 -> False
   | S(x') -> match y with
              | 0 -> True
              | S(y') -> geqNat x' y'
;;
let rec ltNat x y =
  match y with
   | 0 -> False
   | S(y') -> match x with
        | 0 -> True
        | S(x') -> ltNat x' y'
;;
let rec gtNat x y =
  match x with
   | 0 -> False
   | S(x') -> match y with
             | 0 -> True
             | S(y') -> gtNat x' y'

;;
type bool = True | False
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type nat = 0 | S of nat
;;
type Unit = Unit

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
        | S(x) -> match n with
          | 0 -> m
          | S(y) -> minus' x y
  in Pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c

;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)
;;
let rec mult n m = match n with
   | 0 -> 0
   | S(x) -> S(plus (mult x m) m)
;;
type ('a,'b) pair = Pair of 'a * 'b


(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   bigints.raml
 *
 * Author:
 *   Jan Hoffmann (S(S(0))017)
 *
 * Description:
 *   Implementation of arbitrary precision integers.
 *
*)

;;
type bigint = Int of nat list
;;
let word_size = S(S(S(S(S(S(S(S(S(S(0)))))))))) (* use S(S(0))147483648 = S(S(0))^31 for OCaml's '32 bit' ints *)

(* The bigint (Cons(n_0,...,n_k,Nil)) represents the sum \sum_i n_i * w^i
 * where w = word_size.
*)

(* We count the number of integer additions and multiplications in the code
 * by using the tick metric
 *)
;;
let iplus n m = plus n m
;;
let imult n m = mult n m
;;
let split n = match div_mod n word_size with
  | Triple(dv,md,unused) -> Pair (md,dv)

;;
let of_int n = (Cons(n,Nil))

;;
let is_int b =
  match b with
  | Nil()-> true
  | Cons(n,ns) ->
    match ns with
    | Nil()  -> true
    | Cons(x,y) -> false

;;
let to_int_opt b =
  match b with
  | Nil()-> Some 0
  | Cons(n,ns) ->
    match ns with
    | Nil()  -> Some n
    | Cons(x,y) -> None
;;

(*adding an int to a bigint*)
let rec add_int b n =
  ite (eqNat n 0) b
    (match b with
    | Nil()-> (Cons(n,Nil))
    | Cons(m,ms) ->
      match (split (iplus n m)) with
        | Pair(r,d) ->
      Cons(r,(add_int ms d)))

;;
(*adding two bigints*)
let add b c =

  let rec add b c carry =
      match b with
      | Nil()-> add_int c carry
      | Cons(n,ns) ->
        match c with
        | Nil()-> add_int b carry
        | Cons(m,ms) ->
          let sum = iplus (iplus n m) carry in
          match (split sum) with
        | Pair(r,d) ->
          Cons(r,(add ns ms d))
  in
  add b c 0

(* multiplying with an int *)
;;
let mult_int b n =

  let rec mult_int b n carry =
    match b with
    | Nil()->
      ite (eqNat carry 0) Nil Cons(carry,Nil)
    | Cons(m,ms) ->
      match (split (iplus (imult n m) carry)) with
        | Pair(r,d) -> Cons(r,mult_int ms n d)
  in mult_int b n 0
;;

(*multiplying two bigints*)
let mult b c =

  let add b c =

    let rec add_int b n =
      ite (eqNat n 0) b
        (match b with
        | Nil()-> error
        | Cons(m,ms) ->
          match (split (iplus n m)) with
        | Pair(r,d) ->
          Cons(r,(add_int ms d)))
    in

    let rec add b c carry =
      match b with
      | Nil()->
        ite (eqNat carry 0) Nil error
      | Cons(n,ns) ->
        match c with
        | Nil()-> add_int b carry
        | Cons(m,ms) ->
          let sum = iplus (iplus n m) carry in
          match (split sum) with
        | Pair(r,d) ->
          Cons(r,(add ns ms d))
    in
    add b c 0
  in

  let rec append b c =
    match b with
    | Nil()-> c
    | Cons(x,xs) -> Cons(x,(append xs c))
  in

  let rec zeros b =
    match b with
    | Nil()-> Nil
    | Cons(x,xs) -> Cons(0,(zeros xs))
  in

  let mk_acc b c =
    zeros (append b c)
  in

  let rec mult b c acc =
    match b with
    | Nil()-> acc
    | Cons(n,ns) ->
      let acc = add acc (mult_int c n) in
      mult ns (Cons(0,c)) acc
  in

  mult b c (mk_acc b c)


(*test*)

(* mult (Cons(9,Cons(9,Nil))) (Cons(0,Cons(0,Cons(0,Cons(S(0),Nil))))) *)

;;
