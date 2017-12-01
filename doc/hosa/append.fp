type 'a option = None | Some of 'a
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type nat = 0 | S of nat
;;
type Unit = Unit

;;
type 'a list = Nil | Cons of 'a * 'a list
;;
let rec app xs ys = match xs with
  | Nil -> ys
  | Cons(x,xs)  -> Cons(x,(app xs ys))











;;
