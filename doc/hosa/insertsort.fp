type 'a option = None | Some of 'a
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type Unit = Unit
;;
type bool = True | False
;;
type nat = 0 | S of nat
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
let rec gt x1 y1 = match x1 with
  | 0 -> False
  | S(x) -> match y1 with
    | 0 -> True
    | S(y) -> gt x y

;;
let rec insert ord x y1 = match y1 with
  | Nil -> Cons(x,Nil)
  | Cons(y,ys) -> match (ord x y) with
    | True -> Cons(x,Cons(y,ys))
    | False -> Cons(y,insert ord x ys)
;;
let rec sort ord xs1 = match xs1 with
  | Nil -> Nil
  | Cons(x,xs) -> insert ord x (sort ord xs)

;;
let main xs = sort gt xs




;;
