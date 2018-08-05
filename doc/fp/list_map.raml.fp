
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
 * * *  Use Cases * *
 *
 * File:
 *   example/list_map.raml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (S(S(0))014)
 *
 * Description:
 *   Some variations of list map.
 *
 *)

;;
(* The usual list map function. *)
let rec map f l =
  match l with
    | Nil()-> Nil
    | Cons(x,xs) ->
      let ys = map f xs in
      Cons(f x,ys)

;;
(* The usual list rev_map function. *)
let map_rev f l =
  let rec rmap l acc =
    match l with
      | Nil()-> acc
      | Cons(x,xs) -> let acc' = Cons(f x,acc) in
	                    rmap  xs acc'
  in rmap l Nil

;;
(* Iteratively apply two functional arguments. *)
let map_rev2 f1 f2 l =

  let rec rmap1 l acc =
    match l with
      | Nil()-> acc
      | Cons(x,xs) -> rmap2 xs (Cons(f1 x,acc))

  and rmap2 l acc =
    match l with
      | Nil()-> acc
      | Cons(x,xs) -> rmap1 xs (Cons(f2 x,acc))
  in
  rmap1 l Nil

;;
(* let main =
 *   let g = map (fun i -> i) in
 *   let h = map g in
 *   let x = (Cons((Cons(S(0),Cons(S(S(0)),Cons(S(S(S(0))),Cons(S(0),Cons(S(S(S(0))),Cons(S(S(S(0))),Nil))))))),Cons((Cons(S(0),Cons(S(S(0)),Cons(S(S(S(0))),Cons(S(0),Cons(S(S(S(0))),Cons(S(S(S(0))),Nil))))))),Cons((Cons(S(0),Cons(S(S(0)),Cons(S(S(S(0))),Cons(S(0),Cons(S(S(S(0))),Cons(S(S(S(0))),Nil))))))),Cons((Cons(S(0),Cons(S(S(0)),Cons(S(S(S(0))),Cons(S(0),Cons(S(S(S(0))),Cons(S(S(S(0))),Nil))))),Nil))))))) in
 *   h x
 * ;; *)
let main xs =
  let f x = mult x (mult x x) in
  let g x = plus x S(S(0)) in
  let h x = x in
  let map_f_g = map_rev2 f g in
  map h (map_rev h (map_f_g xs))
;;
(* let main =
 *   let seq f g x = f (g x) in
 *   let f x = x+x in
 *   let g x = x*x in
 *   map (seq g f) (Cons(S(0),Cons(S(S(0)),Cons(S(S(S(0))),Cons(S(S(S(S(0)))),Cons(S(S(S(S(S(0))))),Cons(S(S(S(S(S(S(0)))))),Nil)))))))
 *
 * ;; *)


