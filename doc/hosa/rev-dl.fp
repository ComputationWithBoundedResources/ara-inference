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
let id z = z

;;
let comp f g z = f (g z)
;;
let cons1 x y = Cons(x,y)

;;
let rec walk xss = match xss with
 | Nil -> id
 | Cons(x,xs) -> comp (walk xs) (cons1 x)

;;
let rev xs = walk xs












;;
