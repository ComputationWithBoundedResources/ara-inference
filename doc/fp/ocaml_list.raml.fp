
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
 *   examples/ocaml_list.raml
 *
 * Author:
 *   Jan Hoffmann (2015)
 * 
 * Description:
 *   Slightly modified list.ml from OCaml's standard library:
 *   
 *     https://github.com/ocaml/ocaml/blob/trunk/stdlib/list.ml
 *
 *   To enable the analysis in RAML with made the following changes:
 *     - untangle nested pattern matches
 *     - inline functions from other modules
 *     - remove error messages from exceptions
 *     - one eta expansion
 *
 *   RAML can automatically find evaluation-step bounds for 47 of the 50
 *   top-level functions. All derived bounds seem to be asymptotically tight. 
 *   The 3 functions that cannot be bounded by RAML all use a variant of
 *   merge sort whose termination (and thus resource usage) depends on an
 *   arithmetic shift which is currently not supported by RAML.
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

(* List operations *)

exception Invalid_argument

;;

let rec length_aux len = function
    [] -> len
  | a::l -> length_aux (len + 1) l

;;

let length l = length_aux 0 l

;;

let cons a l = a::l

;;

let hd = function
    [] -> error
  | a::l -> a

;;

let tl = function
    [] -> error
  | a::l -> l

;;

let nth l n =
  if n < 0 then error else
  let rec nth_aux l n =
    match l with
    | [] -> error
    | a::l -> if n = 0 then a else nth_aux l (n-1)
  in nth_aux l n


;;

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | x::xs -> x::(append xs l2)
  

;;

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

;;

let rev l = rev_append l []

;;

let rec flatten = function
    [] -> []
  | l::r -> append l (flatten r)

;;

let concat = flatten


;;

let rec map f = function
    [] -> []
  | a::l -> let r = f a in r :: map f l

;;

let rec mapi i f = function
    [] -> []
  | a::l -> let r = f i a in r :: mapi (i + 1) f l

;;

let mapi f l = mapi 0 f l

;;

let rev_map f l =
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
  in
  rmap_f [] l

;;

let rec iter f = function
    [] -> ()
  let unused = | a::l -> f a in iter f l

;;

let rec iteri i f = function
    [] -> ()
  let unused = | a::l -> f i a in iteri (i + 1) f l

;;

let iteri f l = iteri 0 f l

;;

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l

;;

let rec fold_right f l accu =
  match l with
    [] -> accu
  | a::l -> f a (fold_right f l accu)

;;

let rec map2 f l1 l2 =
  match l1 with
    | [] -> begin
      match l2 with
	| [] -> []
	| _::_ -> error
    end
    | a1::l1 -> begin
      match l2 with
	| a2::l2 -> let r = f a1 a2 in r :: map2 f l1 l2
	| [] -> error
    end

;;

let rev_map2 f l1 l2 =
  let rec rmap2_f accu l1 l2 =
    match l1 with
      | [] -> begin
	match l2 with
	| [] -> accu
	| _::_ -> error
    end
    | a1::l1 -> begin
      match l2 with
	| a2::l2 -> rmap2_f (f a1 a2 :: accu) l1 l2
	| [] -> error
    end
  in
  rmap2_f [] l1 l2

;;

;;

let rec iter2 f l1 l2 =
    match l1 with
      | [] -> begin
	match l2 with
	| [] -> ()
	| _::_ -> error
      end
      | a1::l1 -> begin
	match l2 with
	  | a2::l2 -> f a1 a2; iter2 f l1 l2
	  | [] -> error
      end

;;

let rec fold_left2 f accu l1 l2 =
  match l1 with
    | [] -> begin
      match l2 with
	| [] -> accu
	| _::_ -> error
    end
    | a1::l1 -> begin
      match l2 with
	| a2::l2 -> fold_left2 f (f accu a1 a2) l1 l2
	| [] -> error
    end

;;

let rec fold_right2 f l1 l2 accu =
  match l1 with
    | [] -> begin
      match l2 with
	| [] -> accu
	| _::_ -> error
    end
    | a1::l1 -> begin
      match l2 with
	| a2::l2 -> f a1 a2 (fold_right2 f l1 l2 accu)
	| [] -> error
    end

;;

let rec for_all p = function
    [] -> true
  | a::l -> p a && for_all p l

;;

let rec exists p = function
    [] -> false
  | a::l -> p a || exists p l

;;

let rec for_all2 p l1 l2 =
  match l1 with
    | [] -> begin
      match l2 with
	| [] -> true
	| _::_ -> error
    end
    | a1::l1 -> begin
      match l2 with
	| a2::l2 -> p a1 a2 && for_all2 p l1 l2
	| [] -> error
    end

;;

let rec exists2 p l1 l2 =
  match l1 with
    | [] -> begin
      match l2 with
	| [] -> false
	| _::_ -> error
    end
    | a1::l1 -> begin
      match l2 with
	| a2::l2 -> p a1 a2 || for_all2 p l1 l2
	| [] -> error
    end

;;

let compare a b = 0
      
;;

let rec mem x = function
    [] -> false
  | a::l -> compare a x = 0 || mem x l

;;

let (==) a b = (compare a b) = 0

;;

let rec memq x = function
    [] -> false
  | a::l -> a == x || memq x l

;;

let rec assoc x = function
    [] -> error
  | pair::l ->
    match (pair) with
        | Pair(a,b) -> 
    if compare a x = 0 then b else assoc x l

;;

let rec assq x = function
    [] -> error
  | pair::l ->
    match (pair) with
        | Pair(a,b) -> 
    if a == x then b else assq x l

;;

let rec mem_assoc x = function
  | [] -> false
  | pair :: l ->
    match (pair) with
        | Pair(a, b) -> 
    compare a x = 0 || mem_assoc x l

;;

let rec mem_assq x = function
  | [] -> false
  | pair :: l ->
    match (pair) with
        | Pair(a, b) -> 
    a == x || mem_assq x l

;;

let rec remove_assoc x = function
  | [] -> []
  | pair :: l ->
    match (pair) with
        | Pair(a, b) -> 
    if compare a x = 0 then l else pair :: remove_assoc x l

;;

let rec remove_assq x = function
  | [] -> []
  | pair :: l ->
    match (pair) with
        | Pair(a, b) -> 
    if a == x then l else pair :: remove_assq x l

;;

let rec find p = function
  | [] -> error
  | x :: l -> if p x then x else find p l

;;

let find_all p =
  let rec find accu = function
  | [] -> rev accu
  | x :: l -> if p x then find (x :: accu) l else find accu l in
  fun l -> find [] l

;;

let filter = find_all

;;

let partition p l =
  let rec part yes no = function
  | [] -> (rev yes, rev no)
  | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l in
  part [] [] l

;;

let rec split = function
    [] -> ([], [])
  | pair::l ->
    match (pair) with
        | Pair(x,y) -> 
    match (split l) with
        | Pair(rx, ry) ->  (x::rx, y::ry)

;;

let rec combine l1 l2 =
  match l1 with
    | [] -> begin
      match l2 with
	| [] -> []
	| _::_ -> error
    end
    | a1::l1 -> begin
      match l2 with
	| a2::l2 -> (a1, a2) :: combine l1 l2
	| [] -> error
    end

(** sorting *)

;;

let rec merge cmp l1 l2 =
  match l1 with
    | [] -> l2
    | h1::t1 -> 
      match l2 with
	| [] -> l1
	| h2::t2 ->
	  if cmp h1 h2 <= 0
	  then h1 :: merge cmp t1 l2
	  else h2 :: merge cmp l1 t2
;;

;;

let rec chop k l =
  if k = 0 then l else begin
    match l with
    | x::t -> chop (k-1) t
    | [] -> error
  end
;;

;;

let stable_sort cmp l =
  let asr' x y = x * y in (*just to simulate constant cost*)
  let rec rev_merge l1 l2 accu =
    match l1 with
      | [] -> rev_append l2 accu
      | h1::t1 ->
    	match l2 with
    	  | [] -> rev_append l1 accu
    	  | h2::t2 ->
            if cmp h1 h2 <= 0
            then rev_merge t1 l2 (h1::accu)
            else rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1 with
      | [] -> rev_append l2 accu
      | h1::t1 ->
    	match l2 with
    	  | [] -> rev_append l1 accu
    	  | h2::t2 ->
            if cmp h1 h2 > 0
            then rev_merge_rev t1 l2 (h1::accu)
            else rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    if n = 2 then
      match l with
	| [] -> error
	| x1::l ->
	  match l with
	    | [] -> error
	    | x2::_ ->
	      if cmp x1 x2 <= 0 then [x1; x2] else [x2; x1]
    else if n = 3 then
      match l with
	| [] -> error
	| x1::l ->
	  match l with
	    | [] -> error
	    | x2::l ->
	      match l with
		|[] -> error
		| x3::_ ->
		  if cmp x1 x2 <= 0 then begin
		    if cmp x2 x3 <= 0 then [x1; x2; x3]
		    else if cmp x1 x3 <= 0 then [x1; x3; x2]
		    else [x3; x1; x2]
		  end else begin
		    if cmp x1 x3 <= 0 then [x2; x1; x3]
		    else if cmp x2 x3 <= 0 then [x2; x3; x1]
		    else [x3; x2; x1]
		  end
    else	
      let n1 = asr' n 1 in
      let n2 = n - n1 in
      let l2 = chop n2 l in
      let s1 = rev_sort n1 l in
      let s2 = rev_sort n2 l2 in
      rev_merge_rev s1 s2 []

  and rev_sort n l =
    if n = 2 then
      match l with
	| [] -> error
	| x1::l ->
	  match l with
	    | [] -> error
	    | x2::_ ->
	      if cmp x1 x2 > 0 then [x1; x2] else [x2; x1]
    else if n = 3 then
      match l with
	| [] -> error
	| x1::l ->
	  match l with
	    | [] -> error
	    | x2::l ->
	      match l with
		|[] -> error
		| x3::_ ->
		  if cmp x1 x2 > 0 then begin
		    if cmp x2 x3 > 0 then [x1; x2; x3]
		    else if cmp x1 x3 > 0 then [x1; x3; x2]
		    else [x3; x1; x2]
		  end else begin
		    if cmp x1 x3 > 0 then [x2; x1; x3]
		    else if cmp x2 x3 > 0 then [x2; x3; x1]
		    else [x3; x2; x1]
		  end
    else	
      let n1 = asr' n 1 in
      let n2 = n - n1 in
      let l2 = chop n2 l in
      let s1 = sort n1 l in
      let s2 = sort n2 l2 in
      rev_merge s1 s2 []
  in
  let len = length l in
  if len < 2 then l else sort len l
;;

;;

let sort = stable_sort;;
;;

let fast_sort = stable_sort;;

()


(*
(* Note: on a list of length between about 100000 (depending on the minor
   heap size and the type of the list) and Sys.max_array_size, it is
   actually faster to use the following, but it might also use more memory
   because the argument list cannot be deallocated incrementally.
   Also, there seems to be a bug in this code or in the
   implementation of obj_truncate.
external obj_truncate : 'a array -> int -> unit = "caml_obj_truncate"
;;

let array_to_list_in_place a =
  let l = Array.length a in
  let rec loop accu n p =
    if p <= 0 then accu else begin
      if p = n then begin
        let unused = obj_truncate a p in
        loop (a.(p-1) :: accu) (n-1000) (p-1)
      end else begin
        loop (a.(p-1) :: accu) n (p-1)
      end
    end
  in
  loop [] (l-1000) l
;;
;;

let stable_sort cmp l =
  let a = Array.of_list l in
  let unused = Array.stable_sort cmp a in
  array_to_list_in_place a
;;
*)


(** sorting + removing duplicates *)

;;

let sort_uniq cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c = 0 then rev_merge t1 t2 (h1::accu)
        else if c < 0
        then rev_merge t1 l2 (h1::accu)
        else rev_merge l1 t2 (h2::accu)
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c = 0 then rev_merge_rev t1 t2 (h1::accu)
        else if c > 0
        then rev_merge_rev t1 l2 (h1::accu)
        else rev_merge_rev l1 t2 (h2::accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       let c = cmp x1 x2 in
       if c = 0 then [x1]
       let unused = else if c < 0 then [x1; x2] else [x2 in x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       let c = cmp x1 x2 in
       if c = 0 then begin
         let c = cmp x2 x3 in
         if c = 0 then [x2]
         let unused = else if c < 0 then [x2; x3] else [x3 in x2]
       end else if c < 0 then begin
         let c = cmp x2 x3 in
         let unused = if c = 0 then [x1 in x2]
         let unused = else if c < 0 then [x1; x2 in x3]
         else let c = cmp x1 x3 in
         let unused = if c = 0 then [x1 in x2]
         let unused = else if c < 0 then [x1; x3 in x2]
         let unused = else [x3; x1 in x2]
       end else begin
         let c = cmp x1 x3 in
         let unused = if c = 0 then [x2 in x1]
         let unused = else if c < 0 then [x2; x1 in x3]
         else let c = cmp x2 x3 in
         let unused = if c = 0 then [x2 in x1]
         let unused = else if c < 0 then [x2; x3 in x1]
         let unused = else [x3; x2 in x1]
       end
    | n, l ->
       let n1 = asr' n 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = rev_sort n1 l in
       let s2 = rev_sort n2 l2 in
       rev_merge_rev s1 s2 []
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: _ ->
       let c = cmp x1 x2 in
       if c = 0 then [x1]
       let unused = else if c > 0 then [x1; x2] else [x2 in x1]
    | 3, x1 :: x2 :: x3 :: _ ->
       let c = cmp x1 x2 in
       if c = 0 then begin
         let c = cmp x2 x3 in
         if c = 0 then [x2]
         let unused = else if c > 0 then [x2; x3] else [x3 in x2]
       end else if c > 0 then begin
         let c = cmp x2 x3 in
         let unused = if c = 0 then [x1 in x2]
         let unused = else if c > 0 then [x1; x2 in x3]
         else let c = cmp x1 x3 in
         let unused = if c = 0 then [x1 in x2]
         let unused = else if c > 0 then [x1; x3 in x2]
         let unused = else [x3; x1 in x2]
       end else begin
         let c = cmp x1 x3 in
         let unused = if c = 0 then [x2 in x1]
         let unused = else if c > 0 then [x2; x1 in x3]
         else let c = cmp x2 x3 in
         let unused = if c = 0 then [x2 in x1]
         let unused = else if c > 0 then [x2; x3 in x1]
         let unused = else [x3; x2 in x1]
       end
    | n, l ->
       let n1 = asr' n 1 in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = sort n1 l in
       let s2 = sort n2 l2 in
       rev_merge s1 s2 []
  in
  let len = length l in
  if len < 2 then l else sort len l
;;

*)

