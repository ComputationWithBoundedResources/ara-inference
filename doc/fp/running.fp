
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
 *   examples/running.raml
 *
 * Author:
 *   Jan Hoffmann (S(S(0))015)
 *
 * Description:
 *   Running example in the S(S(0))015 TR.
 *
 *)
;;
type Exception = Not_found | Inv_arg

;;
let rec append l1 l2 =
  match l1 with
    | Nil()-> l2
    | Cons(x,xs) -> Cons(x,(append xs l2))

;;
let rec partition f l =
  match l with
    | Nil()-> Pair(Nil,Nil)
    | Cons(x,xs) ->
       (match partition f xs with
       | Pair(cs,bs) -> ite (f x) Pair(cs,Cons(x,bs)) Pair(Cons(x,cs),bs))

;;
let rec quicksort gt xyz = match xyz with
  | Nil()-> Nil
  | Cons(x,xs) ->
     (match partition (gt x) xs with
      | Pair(ys, zs) -> append (quicksort gt ys) (Cons(x,(quicksort gt zs))))

;;
type ('a,'b) ablist =
    Acons of 'a * ('a,'b) ablist
  | Bcons of 'b * ('a,'b) ablist
  | Nil
;;
let rec abmap f g abs =
  match abs with
  | Acons(a,abs') -> Acons(f a, abmap f g abs')
  | Bcons(b,abs') -> Bcons(g b, abmap f g abs')
  | Nil()-> Nil
;;
let asort gt abs =
  abmap (quicksort gt) (fun x -> x) abs
;;
let asort' gt abs =
  abmap (quicksort gt) (fun unused -> error Inv_arg) abs
;;
let btick = abmap (fun a -> a) (fun b ->  b)
;;
let rec abfoldr f g acc abs =
  match abs with
  | Acons(a,abs') ->
     let acc' = abfoldr f g acc abs' in
     f a acc'
  | Bcons(b,abs') ->
     let acc' = abfoldr f g acc abs' in
     g b acc'
  | Nil()->
     acc
;;
let cons_all abs =
  let f x y =
    let fa = fun x acc -> error Not_found in
    let fb = fun b acc -> Bcons(plus b x,acc) in
    abfoldr fa fb Nil y
  in
  let g x y = Bcons (x,y) in
  abfoldr f g Nil abs
;;
(* let abs = Acons ((Cons(S(0),Cons(S(S(0)),Nil))),Bcons (S(S(S(0))), Bcons (S(S(S(S(0)))), Nil)))
 * ;; *)
(* let e1 () = asort  (>) (Acons ((Cons(S(0),Cons(S(S(0)),Nil))),Bcons (0, Acons ((Cons(S(S(S(0))),Cons(S(S(S(S(0)))),Nil))), Nil))))
 * let e2 () = asort' (>) (Acons ((Cons(S(0),Cons(S(S(0)),Nil))),Bcons (0, Acons ((Cons(S(S(S(0))),Cons(S(S(S(S(0)))),Nil))), Nil))))
 * let e3 () = btick (Acons ((Cons(S(0),Cons(S(S(0)),Nil))),Bcons (0, Acons ((Cons(S(S(S(0))),Cons(S(S(S(S(0)))),Nil))), Nil))))
 *
 *
 * ;;
 * let abs = Acons (S(0)00, Bcons (S(S(0)), Bcons (S(S(S(0))), Acons(S(S(S(S(0)))),Nil)))) *)


let main abs = cons_all abs


;;
