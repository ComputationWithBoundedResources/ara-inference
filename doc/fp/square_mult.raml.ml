
let rec leqNat x y =
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
  match x with
   | 0 -> False
   | S(x') -> (match y with
              | 0 -> True
              | S(y') -> geqNat x' y')
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
 *   examples/square_mult.raml
 *
 * Author:
 *   Jan Hoffmann (S(S(0))015)
 *
 * Description:
 *   Squared exponentiation.
 *
 *)
;;
type Exception = Invalid_argument
;;
type binary = Zero | One
;;
type Unit = Unit
;;
let square_mult m k =
(*computes m^k*)

  let x = m in

  let rec sq_mult k = match k with
      | Nil -> Unit
      | Cons(b,bs) -> let x = mult x x
                      in Unit
	in
	let unused =
	  match b with
	    | Zero() -> Unit
	    | One() -> let x = mult x m in sq_mult bs

   match k with
    | Nil()-> error Invalid_argument
    | Cons(b,bs) -> (match b with
	                   | Zero()-> error Invalid_argument
	                   | One()->
	                      let unused = sq_mult bs in
	                      x)


square_mult S(S(S(0))) (Cons(One,Cons(Zero,Cons(One,Cons(One,Cons(Zero,Cons(Zero,Nil)))))))
;;