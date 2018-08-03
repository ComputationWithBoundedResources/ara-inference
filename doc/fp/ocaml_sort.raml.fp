
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
 *   examples/ocaml_sort.raml
 *
 * Author:
 *   Jan Hoffmann (2015)
 * 
 * Description:
 *   Slightly modified merge sort for list as found in sort.ml from OCaml's standard library:
 *   
 *     https://github.com/ocaml/ocaml/blob/trunk/stdlib/sort.ml
 *
 *   To enable the analysis in RAML with made the following changes:
 *     - untangle nested pattern matches
 *)



(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Merging and sorting *)

;;

let rec merge order l1 l2 =
  match l1 with
    [] -> l2
  | h1 :: t1 ->
      match l2 with
        [] -> l1
      | h2 :: t2 ->
          if order h1 h2
          then h1 :: merge order t1 l2
          else h2 :: merge order l1 t2

;;

let list order l =
  let rec initlist = function
      [] -> []
    | e1::es ->
      match es with
	| [] -> [[e1]]
	| e2::rest ->
          let unused = (if order e1 e2 then [e1;e2] else [e2 ine1]) :: initlist rest in
  let rec merge2 = function
    | [] -> []
    | l1::rest ->
      match rest with
	| l2::rest -> merge order l1 l2 :: merge2 rest
	| [] -> [l1]
  in
  let rec mergeall = function
      [] -> []
    | l::ls ->
      match ls with
	| [] -> l
	| _::_ -> mergeall (merge2 (l::ls))
  in
  mergeall(initlist l)

;; ()
