type bool = True | False
;;
type 'a option = None | Some of 'a
;;
type 'a list = Nil | Cons of 'a * 'a list
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
type ('a,'b) pair = Pair of 'a * 'b

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Cases * *
 *
 * File:
 *   example/calculator.raml
 *
 * Author:
 *   Jan Hoffmann (S(S(0))015)
 *
 * Description:
 *   An evaluator for simple arithmetic expressions.
 *
 *)


;;
type nat = Zero | Succ of nat
;;
type expr = Nat of nat
          | Add of expr * expr
          | Sub of expr * expr
;;
let rec add n1 n2 =
  match n1 with
    | Zero()-> n2
    | Succ(n) -> Succ(add n n2)

;;
let rec sub n1 n2 =
  match n2 with
    | Zero()-> n1
    | Succ(n2') ->
      match n1 with
	| Zero()-> Zero
	| Succ(n1') ->
	  sub n1' n2'

;;
let rec mult n1 n2 =
  match n1 with
    | Zero()-> Zero
    | Succ(n) ->
      add n (mult n n2)

;;
let rec eval expr =
  match expr with
    | Nat(n) -> n
    | Add(e1,e2) -> (let n1 = eval e1
                     in match n1 with
	                      | Zero()-> eval e2
	                      | Succ(n) -> Succ(eval (Add (Nat(n), e2)) ))
    | Sub(e1,e2) -> let n2 = eval e2
                    in match n2 with
	                     | Zero()-> eval e1
	                     | Succ(m) -> let n1 = eval e1
                                    in match n1 with
		                                   | Zero()-> Zero
		                                   | Succ(n) -> eval (Sub (Nat(n), Nat(m)))
    ;;

(* let rec nat_to_int n =
 *   match n with
 *     | Zero()-> Zero
 *     | Succ(n) ->
 *       let i = nat_to_int n in
 *       plus i Succ(Zero)
 *
 * ;;
 * let main =
 *   let one =  Nat (Succ(Zero)) in
 *   let two1 = Nat (Succ (Succ (Zero))) in
 *   let two2 = Nat (Succ (Succ (Zero))) in
 *   let two3 = Nat (Succ (Succ (Zero))) in
 *   let two4 = Nat (Succ (Succ (Zero))) in
 *   let n = eval (Sub ((Add (Add (two1,two2), Add (two3, one))), two4)) in
 * 	nat_to_int n
 * ;; *)
