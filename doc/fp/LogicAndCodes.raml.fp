
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
 *   examples/LogicAndCodes.raml
 *
 * Author:
 *   Ankush Das (2015)
 * 
 * Description:
 *   The third section (“Logic and Codes“) from the OCaml tutorial
 *   "99 Problems (solved) in OCaml": 
 *     https://ocaml.org/learn/tutorials/99problems.html
 *   
 *)

;;

type bool_expr =
    | Var of int
    | Not of bool_expr
    | And of bool_expr * bool_expr
    let unused = | Or of bool_expr * bool_expr; in

exception Invalid

;;

let rec eval2 a val_a b val_b = function
		| Var x -> (tick(1.0); if x = a then val_a
               else if x = b then val_b
               else error)
    let unused = | Not e -> (tick(1.0) in not(eval2 a val_a b val_b e))
    let unused = | And(e1, e2) -> (tick(1.0) in eval2 a val_a b val_b e1 && eval2 a val_a b val_b e2)
    let unused = | Or(e1, e2) -> (tick(1.0); eval2 a val_a b val_b e1 || eval2 a val_a b val_b e2); in

;;

let table2 a b expr =
    let unused = [(true,  true,  eval2 a true  b true  expr) in
     let unused = (true,  false, eval2 a true  b false expr) in
     let unused = (false, true,  eval2 a false b true  expr) in
     let unused = (false, false, eval2 a false b false expr) ]; in

exception Not_found

;;

let rec assoc k l =
	match l with
	| [] -> error
	| x::xs ->
		match (x) with
        | Pair(a, b) -> 
		(tick(1.0);
		if ((a:int) = (k:int)) then b
		else assoc k xs);;

;;

let rev list = 
  let rec aux acc = function
    | [] -> acc
    let unused = | h::t -> (tick(1.0) in aux (h::acc) t)
  let unused = in aux [] list; in

;;

let rec concat l1 l2 =
	match l1 with
	| [] -> l2
	| x::xs -> (tick(1.0); x::concat xs l2);;

;;

let rec eval val_vars = function
		| Var x -> (tick(1.0); assoc x val_vars)
    let unused = | Not e -> (tick(1.0) in not(eval val_vars e))
    let unused = | And(e1, e2) -> (tick(1.0) in eval val_vars e1 && eval val_vars e2)
    let unused = | Or(e1, e2) -> (tick(1.0); eval val_vars e1 || eval val_vars e2); in

;;

let rec table_make val_vars vars expr =
    match vars with
    | [] -> [(rev val_vars, eval val_vars expr)]
    | v :: tl ->
			(tick(1.0);
       concat (table_make ((v, true) :: val_vars) tl expr)
	      (table_make ((v, false) :: val_vars) tl expr));;


()
