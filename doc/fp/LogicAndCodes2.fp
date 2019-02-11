
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
 *   examples/LogicAndCodes.raml
 *
 * Author:
 *   Ankush Das (S(S(0))015)
 *
 * Description:
 *   The third section (“Logic and Codes“) from the OCaml tutorial
 *   "99 Problems (solved) in OCaml":
 *     https://ocaml.org/learn/tutorials/99problems.html
 *
 *)
;;
type bool = True | False
;;
type bool_expr = Var of nat
               | Not of bool_expr
               | And of bool_expr * bool_expr
               | Or of bool_expr * bool_expr
;;
type Exception = Invalid | Not_found

;;
let lnot b = match b with
  | True -> False
  | False -> True
;;
let land a b = match a with
           | False -> False
           | True -> b
;;
let lor a b = match a with
  | True -> True
  | False -> b

;;
let rec eval2 a val_a b val_b xyz = match xyz with
		| Var(x) -> (ite (eqNat x a) val_a (ite (eqNat x b) val_b (error Invalid)))
    | Not(e) ->  lnot (eval2 a val_a b val_b e)
    | And(e1, e2) -> land (eval2 a val_a b val_b e1) (eval2 a val_a b val_b e2)
    | Or(e1, e2) -> lor (eval2 a val_a b val_b e1) (eval2 a val_a b val_b e2)
;;

let table2 a b expr =
    Cons(Triple(True,  True,  eval2 a True  b True  expr),
    Cons(Triple(True,  False, eval2 a True  b False expr),
    Cons(Triple(False, True,  eval2 a False b True  expr),
    Cons(Triple(False, False, eval2 a False b False expr),Nil))))

;;


let main a b expr = table2 a b expr;;
