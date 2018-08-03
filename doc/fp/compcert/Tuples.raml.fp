
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
;;
;;

type __
;;
;;

type 'x arrows_left = __
;;
;;

type 'x arrows_right = __
;;
;;

type tuple = __
;;
;;

let rec magic x = magic x

(** val uncurry : __ list -> 'a1 arrows_left -> tuple -> 'a1 **)
;;
;;

let rec uncurry args f x =
  match args with
  | Nil -> magic f
  | _Cons(,q) -> let(d, t) = magic x in magic uncurry q f t d

 ()
;;
