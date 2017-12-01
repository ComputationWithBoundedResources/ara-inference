type 'a option = None | Some of 'a
;;
type nat = 0 | S of nat
;;
type Unit = Unit

;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type ('a,'b) pair = Pair of 'a * 'b

;;
let rec foldr f b xs = match xs with
  | Nil -> b
  | Cons(l,ls) -> f l (foldr f b ls)

;;
let lam1 n m l = Cons(Pair(n,m),l)
;;
let lam2 ms n l = foldr (lam1 n) l ms

;;
let product ns ms = foldr (lam2 ms) Nil ns












;;
