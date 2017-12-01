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
let tail xss = match xss with
 | Cons(x,xs) -> xs

;;
let rec scheme f xss = match xss with
 | Nil -> Nil
 | Cons(x,xs) -> scheme f (f xs)

;;
let main l = scheme tail l










;;
