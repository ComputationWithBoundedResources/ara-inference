
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

type nat = 0 | S of nat
;;
;;

type Unit = Unit

(** val string_dec : char(list) -> char(list) -> bool **)
;;
;;

let rec string_dec s s0 =
  match s with
  | Nil ->
    (match s0 with
     | Nil -> true
     | Cons(a,s1) -> false)
  | Cons(a,s1) ->
    (match s0 with
     | Nil -> false
     | Cons(a0,s2) -> if ((a) = (a0) ) then string_dec s1 s2 else false)

(** val prefix : char(list) -> char(list) -> bool **)
;;
;;

let rec prefix s1 s2 =
  match s1 with
  | Nil -> true
  | Cons(a,s1') ->
    (match s2 with
     | Nil -> false
     | Cons(b,s2') -> if ((a) = (b)) then prefix s1' s2' else false)

 ()
;;
