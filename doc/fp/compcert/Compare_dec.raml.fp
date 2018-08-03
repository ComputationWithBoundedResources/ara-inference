
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

;;

type 'a option = None | Some of 'a
;;
;;

type ('a,'b) pair = Pair of 'a * 'b
;;
;;

type 'a list = Nil | Cons of 'a * 'a list
;;
;;

type Unit = Unit

(*-- Datatypes.ml --*)
(** val implb : bool -> bool -> bool **)
;;
;;

let implb b1 b2 =
  if b1 then b2 else true

(** val xorb : bool -> bool -> bool **)
;;
;;

let xorb b1 b2 =
  if b1 then if b2 then false else true else b2

(** val negb : bool -> bool **)
;;
;;

letnegb = match negb with(x) ->
if (x = true) then false else true
;;
;;

type nat = O
| S of nat
;;
;;

type ('a, 'b) sum =
| Coq_inl of 'a
| Coq_inr of 'b

(** val fst : ('a1 * 'a2) -> 'a1 **)
;;
;;

letfst = match fst with(a) ->
;;

let(x, y) = a in x

(** val snd : ('a1 * 'a2) -> 'a2 **)
;;
;;

letsnd = match snd with(a) ->
;;

let(x, y) = a in y

(** val length : 'a1(list) -> nat **)
;;
;;

let reclength = match length with(myvariable) ->
match myvariable with
| Nil -> O
| Cons(y,l') -> S (length l')

(** val app : 'a1(list) -> 'a1(list) -> 'a1 list **)
;;
;;

let rec app l m =
  match l with
  | Nil -> m
  | Cons(a,l1) -> Cons(a,(app l1 m))
;;
;;

type comparison = Eq
| Lt
| Gt

(** val coq_CompOpp : comparison -> comparison **)
;;
;;

let coq_CompOpp = function(myvariable) ->
match myvariable with
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt
;;
;;

type coq_CompareSpecT =
| CompEqT
| CompLtT
| CompGtT

(** val coq_CompareSpec2Type : comparison -> coq_CompareSpecT **)
;;
;;

let coq_CompareSpec2Type = function(myvariable) ->
match myvariable with
| Eq -> CompEqT
| Lt -> CompLtT
| Gt -> CompGtT
;;
;;

type 'a coq_CompSpecT = coq_CompareSpecT

(** val coq_CompSpec2Type : 'a1 -> 'a1 -> comparison -> 'a1 coq_CompSpecT **)
;;
;;

let coq_CompSpec2Type x y c =
  coq_CompareSpec2Type c

(*$$ Datatypes.ml $$*)

(** val le_lt_dec : nat -> nat -> bool **)
;;
;;

let rec le_lt_dec n m =
  match n with
  | O -> true
  | S(n0) ->
    (match m with
     | O -> false
     | S(m0) -> le_lt_dec n0 m0)

(** val le_gt_dec : nat -> nat -> bool **)
;;
;;

let le_gt_dec n m =
  le_lt_dec n m

(** val nat_compare : nat -> nat -> comparison **)
;;
;;

let rec nat_compare n m =
  match n with
  | O ->
    (match m with
     | O -> Eq
     | S(n0) -> Lt)
  | S(n') ->
    (match m with
     | O -> Gt
     | S(m') -> nat_compare n' m')

 ()
;;
