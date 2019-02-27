
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

let rec assoc k l =
	match l with
	| Nil()-> error Not_found
	| Cons(x,xs) -> (match  x with
                   | Pair(a,b) ->
		                  (ite (eqNat a k) b (assoc k xs)))
;;
let rev list =
  let rec aux acc xyz = match xyz with
    | Nil()-> acc
    | Cons(h,t) -> aux (Cons(h,acc)) t
  in aux Nil list
;;
let rec concat l1 l2 =
	match l1 with
	| Nil()-> l2
	| Cons(x,xs) -> Cons(x,concat xs l2)
;;
let rec eval val_vars xyz = match xyz with
		| Var(x) -> assoc x val_vars
    | Not(e) -> lnot(eval val_vars e)
    | And(e1, e2) -> land (eval val_vars e1) (eval val_vars e2)
    | Or(e1, e2) -> lor (eval val_vars e1) (eval val_vars e2)
;;
let rec table_make val_vars vars expr =
    match vars with
    | Nil()-> Cons(Pair(rev val_vars, eval val_vars expr),Nil)
    | Cons(v,tl) -> concat (table_make (Cons(Pair(v, True),val_vars)) tl expr)
	                    (table_make (Cons(Pair(v, False),val_vars)) tl expr)

;;
