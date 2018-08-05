
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
  in minus' n m
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
 *   examples/hello_world.raml
 *
 * Author:
 *   Jan Hoffmann (S(S(0))015)
 *
 * Description:
 *   Hello world with ASCII.
 *)

;;
let rec append l1 l2 =
  match l1 with
    | Nil()-> l2
    | Cons(x,xs) -> Cons(x,append xs l2)
;;
(*
 *
 *
 * ;;
 * *)
let main =
  let s5 = S(S(S(S(S(0))))) in
  let s10 = S(S(S(S(S(S(S(S(S(S(0)))))))))) in
  let s20 = plus s10 s10 in
  let s50 = plus (plus s20 s20) s10 in
  let s70 = plus s50 s20 in
  let s100 = plus s50 s50 in
  let s72 = plus s70 (S(S(0))) in
  let s101 = plus s100 S(0) in
  let s108 = plus (plus s100 s5) S(S(S(0))) in
  let s111 = plus (plus s100 s10) S(0) in
  let s87 = minus (plus s70 s10) S(S(S(0))) in
  let s114 = plus s111 S(S(S(0))) in
  let s32 = plus (plus s20 s10) S(S(0)) in
  let s33 = plus s32 S(0) in

  let hello = Cons(s72,Cons(s101,Cons(s108,Cons(s108,Cons(s111,Nil))))) in
  let world = Cons(s87,Cons(s111,Cons(s114,Cons(s108,Cons(s100,Nil))))) in
  let hello' = append hello Cons(s32,Nil) in
  let world' = append world Cons(s33,Nil) in
  append hello' world'

;;
