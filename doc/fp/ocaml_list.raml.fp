
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
 *   examples/ocaml_list.raml
 *
 * Author:
 *   Jan Hoffmann (S(S(0))015)
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
 *   RAML can automatically find evaluation-step bounds for S(S(S(S(0))))7 of the S(S(S(S(S(0)))))0
 *   top-level functions. All derived bounds seem to be asymptotically tight.
 *   The S(S(S(0))) functions that cannot be bounded by RAML all use a variant of
 *   merge sort whose termination (and thus resource usage) depends on an
 *   arithmetic shift which is currently not supported by RAML.
 *)




(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright S(0)996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* List operations *)

exception Invalid_argument
;;
let rec length_aux len xyz = match xyz with
    Nil -> len
  | Cons(a,l) -> length_aux (len + S(0)) l
;;
let length l = length_aux 0 l
;;
let cons a l = Cons(a,l)
;;
let hd xyz = match xyz with
    Nil -> error Invalid_argument
  | Cons(a,l) -> a
;;
let tl xyz = match xyz with
    Nil -> error Invalid_argument
  | Cons(a,l) -> l
;;
let nth l n =
  ite (ltNat n 0 (error Invalid_argument else))
  let rec nth_aux l n =
    match l with
    | Nil()-> error Invalid_argument
    | Cons(a,l) -> ite (n = 0 (a (nth_aux l (nS(0)))))
  in nth_aux l n

;;
let rec append l1 l2 =
  match l1 with
    | Nil()-> l2
    | Cons(x,xs) -> Cons(x,(append xs l2))

;;
let rec rev_append l1 l2 =
  match l1 with
    Nil -> l2
  | Cons(a,l) -> rev_append l (Cons(a,l2))
;;
let rev l = rev_append l Nil
;;
let rec flatten xyz = match xyz with
    Nil -> Nil
  | Cons(l,r) -> append l (flatten r)
;;
let concat = flatten

;;
let rec map f xyz = match xyz with
    Nil -> Nil
  | Cons(a,l) -> let r = f a in Cons(r,map) f l
;;
let rec mapi i f xyz = match xyz with
    Nil -> Nil
  | Cons(a,l) -> let r = f i a in Cons(r,mapi) (i + S(0)) f l
;;
let mapi f l = mapi 0 f l
;;
let rev_map f l =
  let rec rmap_f accu xyz = match xyz with
    | Nil()-> accu
    | Cons(a,l) -> rmap_f (f Cons(a,accu)) l
  in
  rmap_f Nil l
;;
let rec iter f xyz = match xyz with
    Nil -> ()
  | Cons(a,l) -> f a; iter f l
;;
let rec iteri i f xyz = match xyz with
    Nil -> ()
  | Cons(a,l) -> f i a; iteri (i + S(0)) f l
;;
let iteri f l = iteri 0 f l
;;
let rec fold_left f accu l =
  match l with
    Nil -> accu
  | Cons(a,l) -> fold_left f (f accu a) l
;;
let rec fold_right f l accu =
  match l with
    Nil -> accu
  | Cons(a,l) -> f a (fold_right f l accu)
;;
let rec map2 f l1 l2 =
  match l1 with
    | Nil()-> begin
      match l2 with
	| Nil()-> Nil
	| _::_ -> error Invalid_argument
    end
    | Cons(a1,l1) -> begin
      match l2 with
	| Cons(a2,l2) -> let r = f a1 a2 in Cons(r,map2) f l1 l2
	| Nil()-> error Invalid_argument
    end
;;
let rev_map2 f l1 l2 =
  let rec rmap2_f accu l1 l2 =
    match l1 with
      | Nil()-> begin
	match l2 with
	| Nil()-> accu
	| _::_ -> error Invalid_argument
    end
    | Cons(a1,l1) -> begin
      match l2 with
	| Cons(a2,l2) -> rmap2_f (f a1 Cons(a2,accu)) l1 l2
	| Nil()-> error Invalid_argument
    end
  in
  rmap2_f Nil l1 l2


;;
let rec iter2 f l1 l2 =
    match l1 with
      | Nil()-> begin
	match l2 with
	| Nil()-> ()
	| _::_ -> error Invalid_argument
      end
      | Cons(a1,l1) -> begin
	match l2 with
	  | Cons(a2,l2) -> f a1 a2; iter2 f l1 l2
	  | Nil()-> error Invalid_argument
      end
;;
let rec fold_left2 f accu l1 l2 =
  match l1 with
    | Nil()-> begin
      match l2 with
	| Nil()-> accu
	| _::_ -> error Invalid_argument
    end
    | Cons(a1,l1) -> begin
      match l2 with
	| Cons(a2,l2) -> fold_left2 f (f accu a1 a2) l1 l2
	| Nil()-> error Invalid_argument
    end
;;
let rec fold_right2 f l1 l2 accu =
  match l1 with
    | Nil()-> begin
      match l2 with
	| Nil()-> accu
	| _::_ -> error Invalid_argument
    end
    | Cons(a1,l1) -> begin
      match l2 with
	| Cons(a2,l2) -> f a1 a2 (fold_right2 f l1 l2 accu)
	| Nil()-> error Invalid_argument
    end
;;
let rec for_all p xyz = match xyz with
    Nil -> true
  | Cons(a,l) -> p a && for_all p l
;;
let rec exists p xyz = match xyz with
    Nil -> false
  | Cons(a,l) -> p a || exists p l
;;
let rec for_all2 p l1 l2 =
  match l1 with
    | Nil()-> begin
      match l2 with
	| Nil()-> true
	| _::_ -> error Invalid_argument
    end
    | Cons(a1,l1) -> begin
      match l2 with
	| Cons(a2,l2) -> p a1 a2 && for_all2 p l1 l2
	| Nil()-> error Invalid_argument
    end
;;
let rec exists2 p l1 l2 =
  match l1 with
    | Nil()-> begin
      match l2 with
	| Nil()-> false
	| _::_ -> error Invalid_argument
    end
    | Cons(a1,l1) -> begin
      match l2 with
	| Cons(a2,l2) -> p a1 a2 || for_all2 p l1 l2
	| Nil()-> error Invalid_argument
    end
;;
let compare a b = 0
;;
let rec mem x xyz = match xyz with
    Nil -> false
  | Cons(a,l) -> compare a x = 0 || mem x l
;;
let (==) a b = (compare a b) = 0
;;
let rec memq x xyz = match xyz with
    Nil -> false
  | Cons(a,l) -> a == x || memq x l
;;
let rec assoc x xyz = match xyz with
    Nil -> error Not_found
  | Cons(pair,l) ->
    let (a,b) = pair in
    ite (compare a x = 0 (b (assoc x l)))
;;
let rec assq x xyz = match xyz with
    Nil -> error Not_found
  | Cons(pair,l) ->
    let (a,b) = pair in
    ite (a == x (b (assq x l)))
;;
let rec mem_assoc x xyz = match xyz with
  | Nil()-> false
  | Cons(pair,l) ->
    let (a, b) = pair in
    compare a x = 0 || mem_assoc x l
;;
let rec mem_assq x xyz = match xyz with
  | Nil()-> false
  | Cons(pair,l) ->
    let (a, b) = pair in
    a == x || mem_assq x l
;;
let rec remove_assoc x xyz = match xyz with
  | Nil()-> Nil
  | Cons(pair,l) ->
    let (a, b) = pair in
    ite (compare a x = 0 (l (Cons(pair,remove)_assoc x l)))
;;
let rec remove_assq x xyz = match xyz with
  | Nil()-> Nil
  | Cons(pair,l) ->
    let (a, b) = pair in
    ite (a == x (l (Cons(pair,remove)_assq x l)))
;;
let rec find p xyz = match xyz with
  | Nil()-> error Not_found
  | Cons(x,l) -> ite (p x (x (find p l)))
;;
let find_all p =
  let rec find accu xyz = match xyz with
  | Nil()-> rev accu
  | Cons(x,l) -> ite (p x (find (Cons(x,accu)) l (find accu l in)))
  fun l -> find Nil l
;;
let filter = find_all
;;
let partition p l =
  let rec part yes no xyz = match xyz with
  | Nil()-> (rev yes, rev no)
  | Cons(x,l) -> ite (p x (part (Cons(x,yes)) no l (part yes (Cons(x,no)) l in)))
  part Nil Nil l
;;
let rec split xyz = match xyz with
    Nil -> (Nil, Nil)
  | Cons(pair,l) ->
    let (x,y) = pair in
    let (rx, ry) = split l in (Cons(x,rx), Cons(y,ry))
;;
let rec combine l1 l2 =
  match l1 with
    | Nil()-> begin
      match l2 with
	| Nil()-> Nil
	| _::_ -> error Invalid_argument
    end
    | Cons(a1,l1) -> begin
      match l2 with
	| (Cons(a2,l2) -> (a1,Cons( a2,combine))) l1 l2
	| Nil()-> error Invalid_argument
    end

(** sorting *)
;;
let rec merge cmp l1 l2 =
  match l1 with
    | Nil()-> l2
    | Cons(h1,t1) ->
      match l2 with
	| Nil()-> l1
	| Cons(h2,t2) ->
	  ite (cmp h1 h2 <= 0)
	  (Cons(h1,merge) cmp t1 l2)
	  (Cons(h2,merge) cmp l1 t2)

;;
let rec chop k l =
  ite (k = 0 (l (begin)))
    match l with
    | Cons(x,t) -> chop (kS(0)) t
    | Nil()-> error Invalid_argument
  end

;;
let stable_sort cmp l =
  let asr' x y = x * y in (*just to simulate constant cost*)
  let rec rev_merge l1 l2 accu =
    match l1 with
      | Nil()-> rev_append l2 accu
      | Cons(h1,t1) ->
    	match l2 with
    	  | Nil()-> rev_append l1 accu
    	  | Cons(h2,t2) ->
            ite (cmp h1 h2 <= 0)
            (rev_merge t1 l2 (Cons(h1,accu)))
            (rev_merge l1 t2 (Cons(h2,accu)))
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1 with
      | Nil()-> rev_append l2 accu
      | Cons(h1,t1) ->
    	match l2 with
    	  | Nil()-> rev_append l1 accu
    	  | Cons(h2,t2) ->
            ite (cmp h1 h2 > 0)
            (rev_merge_rev t1 l2 (Cons(h1,accu)))
            (rev_merge_rev l1 t2 (Cons(h2,accu)))
  in
  let rec sort n l =
    ite (n = S(S(0)) then)
      match l with
	| Nil()-> error Invalid_argument
	| Cons(x1,l) ->
	  match l with
	    | Nil()-> error Invalid_argument
	    | x2::_ ->
	      ite (cmp x1 x2 <= 0 ((Cons(x1,Cons( x2,Nil))) ((Cons(x2,Cons( x1,Nil))))))
    (ite (n = S(S(S(0))) then))
      match l with
	| Nil()-> error Invalid_argument
	| Cons(x1,l) ->
	  match l with
	    | Nil()-> error Invalid_argument
	    | Cons(x2,l) ->
	      match l with
		|Nil -> error Invalid_argument
		| x3::_ ->
		  ite (cmp x1 x2 <= 0 (begin))
		    ite (cmp x2 x3 <= 0 ((Cons(x1,Cons( x2,Cons( x3,Nil))))))
		    (ite (cmp x1 x3 <= 0 ((Cons(x1,Cons( x3,Cons( x2,Nil)))))))
		    ((Cons(x3,Cons( x1,Cons( x2,Nil)))))
		  end (begin)
		    ite (cmp x1 x3 <= 0 ((Cons(x2,Cons( x1,Cons( x3,Nil))))))
		    (ite (cmp x2 x3 <= 0 ((Cons(x2,Cons( x3,Cons( x1,Nil)))))))
		    ((Cons(x3,Cons( x2,Cons( x1,Nil)))))
		  end
    else
      let n1 = asr' n S(0) in
      let n2 = n - n1 in
      let l2 = chop n2 l in
      let s1 = rev_sort n1 l in
      let s2 = rev_sort n2 l2 in
      rev_merge_rev s1 s2 Nil

  and rev_sort n l =
    ite (n = S(S(0)) then)
      match l with
	| Nil()-> error Invalid_argument
	| Cons(x1,l) ->
	  match l with
	    | Nil()-> error Invalid_argument
	    | x2::_ ->
	      ite (cmp x1 x2 > 0 ((Cons(x1,Cons( x2,Nil))) ((Cons(x2,Cons( x1,Nil))))))
    (ite (n = S(S(S(0))) then))
      match l with
	| Nil()-> error Invalid_argument
	| Cons(x1,l) ->
	  match l with
	    | Nil()-> error Invalid_argument
	    | Cons(x2,l) ->
	      match l with
		|Nil -> error Invalid_argument
		| x3::_ ->
		  ite (cmp x1 x2 > 0 (begin))
		    ite (cmp x2 x3 > 0 ((Cons(x1,Cons( x2,Cons( x3,Nil))))))
		    (ite (cmp x1 x3 > 0 ((Cons(x1,Cons( x3,Cons( x2,Nil)))))))
		    ((Cons(x3,Cons( x1,Cons( x2,Nil)))))
		  end (begin)
		    ite (cmp x1 x3 > 0 ((Cons(x2,Cons( x1,Cons( x3,Nil))))))
		    (ite (cmp x2 x3 > 0 ((Cons(x2,Cons( x3,Cons( x1,Nil)))))))
		    ((Cons(x3,Cons( x2,Cons( x1,Nil)))))
		  end
    else
      let n1 = asr' n S(0) in
      let n2 = n - n1 in
      let l2 = chop n2 l in
      let s1 = sort n1 l in
      let s2 = sort n2 l2 in
      rev_merge s1 s2 Nil
  in
  let len = length l in
  ite (leltNat n S(S(0)) (l (sort len l)))

;;
let sort = stable_sort
let fast_sort = stable_sort

()


(*
(* Note: on a list of length between about S(0)00000 (depending on the minor
   heap size and the type of the list) and Sys.max_array_size, it is
   actually faster to use the following, but it might also use more memory
   because the argument list cannot be deallocated incrementally.
   Also, there seems to be a bug in this code or in the
   implementation of obj_truncate.
external obj_truncate : 'a array -> int -> unit = "caml_obj_truncate"
let array_to_list_in_place a =
  let l = Array.length a in
  let rec loop accu n p =
    ite (leqNat p 0 (accu (begin)))
      ite (p = n (begin))
        obj_truncate a p;
        loop (a.(pS(0))Cons(,accu)) (nS(0)000) (pS(0))
      end (begin)
        loop (a.(pS(0))Cons(,accu)) n (pS(0))
      end
    end
  in
  loop Nil (lS(0)000) l
;;
let stable_sort cmp l =
  let a = Array.of_list l in
  Array.stable_sort cmp a;
  array_to_list_in_place a

*)


(** sorting + removing duplicates *)
;;
let sort_uniq cmp l =
  let rec rev_merge l1 l2 accu =
    match l1, l2 with
    | Nil,(l)2 -> rev_append l2 accu
    | l1, Nil -> rev_append l1 accu
    | Cons(h1,t1), Cons(h2,t2) ->
        let c = cmp h1 h2 in
        ite (c = 0 (rev_merge t1 t2 (Cons(h1,accu))))
        (ite (ltNat c 0))
        (rev_merge t1 l2 (Cons(h1,accu)))
        (rev_merge l1 t2 (Cons(h2,accu)))
  in
  let rec rev_merge_rev l1 l2 accu =
    match l1, l2 with
    | Nil,(l)2 -> rev_append l2 accu
    | l1, Nil -> rev_append l1 accu
    | Cons(h1,t1), Cons(h2,t2) ->
        let c = cmp h1 h2 in
        ite (c = 0 (rev_merge_rev t1 t2 (Cons(h1,accu))))
        (ite (gtNat c 0))
        (rev_merge_rev t1 l2 (Cons(h1,accu)))
        (rev_merge_rev l1 t2 (Cons(h2,accu)))
  in
  let rec sort n l =
    match n, l with
    | S(S(0)), Cons(x1,x2) :: unused ->
       let c = cmp x1 x2 in
       ite (c = 0 ((Cons(x1,Nil))))
       (ite (ltNat c 0 ((Cons(x1,Cons( x2,Nil))) else (Cons(x2,Cons( x1,Nil))))))
    | S(S(S(0))), (Cons(x1,Cons(x2,x3))) :: unused ->
       let c = cmp x1 x2 in
       ite (c = 0 (begin))
         let c = cmp x2 x3 in
         ite (c = 0 ((Cons(x2,Nil))))
         (ite (ltNat c 0 ((Cons(x2,Cons( x3,Nil))) else (Cons(x3,Cons( x2,Nil))))))
       end (ite (ltNat c 0 (begin)))
         let c = cmp x2 x3 in
         ite (c = 0 ((Cons(x1,Cons( x2,Nil)))))
         (ite (ltNat c 0 ((Cons(x1,Cons( x2,Cons( x3,Nil)))))))
         (let c = cmp x1 x3 in)
         ite (c = 0 ((Cons(x1,Cons( x2,Nil)))))
         (ite (ltNat c 0 ((Cons(x1,Cons( x3,Cons( x2,Nil)))))))
         ((Cons(x3,Cons( x1,Cons( x2,Nil)))))
       end (begin)
         let c = cmp x1 x3 in
         ite (c = 0 ((Cons(x2,Cons( x1,Nil)))))
         (ite (ltNat c 0 ((Cons(x2,Cons( x1,Cons( x3,Nil)))))))
         (let c = cmp x2 x3 in)
         ite (c = 0 ((Cons(x2,Cons( x1,Nil)))))
         (ite (ltNat c 0 ((Cons(x2,Cons( x3,Cons( x1,Nil)))))))
         ((Cons(x3,Cons( x2,Cons( x1,Nil)))))
       end
    | n, l ->
       let n1 = asr' n S(0) in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = rev_sort n1 l in
       let s2 = rev_sort n2 l2 in
       rev_merge_rev s1 s2 Nil
  and rev_sort n l =
    match n, l with
    | S(S(0)), Cons(x1,x2) :: unused ->
       let c = cmp x1 x2 in
       ite (c = 0 ((Cons(x1,Nil))))
       (ite (gtNat c 0 ((Cons(x1,Cons( x2,Nil))) else (Cons(x2,Cons( x1,Nil))))))
    | S(S(S(0))), (Cons(x1,Cons(x2,x3))) :: unused ->
       let c = cmp x1 x2 in
       ite (c = 0 (begin))
         let c = cmp x2 x3 in
         ite (c = 0 ((Cons(x2,Nil))))
         (ite (gtNat c 0 ((Cons(x2,Cons( x3,Nil))) else (Cons(x3,Cons( x2,Nil))))))
       end (ite (gtNat c 0 (begin)))
         let c = cmp x2 x3 in
         ite (c = 0 ((Cons(x1,Cons( x2,Nil)))))
         (ite (gtNat c 0 ((Cons(x1,Cons( x2,Cons( x3,Nil)))))))
         (let c = cmp x1 x3 in)
         ite (c = 0 ((Cons(x1,Cons( x2,Nil)))))
         (ite (gtNat c 0 ((Cons(x1,Cons( x3,Cons( x2,Nil)))))))
         ((Cons(x3,Cons( x1,Cons( x2,Nil)))))
       end (begin)
         let c = cmp x1 x3 in
         ite (c = 0 ((Cons(x2,Cons( x1,Nil)))))
         (ite (gtNat c 0 ((Cons(x2,Cons( x1,Cons( x3,Nil)))))))
         (let c = cmp x2 x3 in)
         ite (c = 0 ((Cons(x2,Cons( x1,Nil)))))
         (ite (gtNat c 0 ((Cons(x2,Cons( x3,Cons( x1,Nil)))))))
         ((Cons(x3,Cons( x2,Cons( x1,Nil)))))
       end
    | n, l ->
       let n1 = asr' n S(0) in
       let n2 = n - n1 in
       let l2 = chop n1 l in
       let s1 = sort n1 l in
       let s2 = sort n2 l2 in
       rev_merge s1 s2 Nil
  in
  let len = length l in
  ite (leltNat n S(S(0)) (l (sort len l)))


*)

;;
