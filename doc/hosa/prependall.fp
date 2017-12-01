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
let rec map f xss = match xss with
  | Nil -> Nil
  | Cons(x,xs) -> Cons(f x, map f xs)
;;
let rec app xss ys = match xss with
  | Nil -> ys
  | Cons(x,xs) -> Cons(x, app xs ys)
;;
let prepAll xs ys = map (app xs) ys



;;
