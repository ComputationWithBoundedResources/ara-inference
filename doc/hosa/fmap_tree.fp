type 'a option = None | Some of 'a
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type Unit = Unit

;;
type nat = 0 | S of nat
;;
type 'a Tree = Leaf of 'a | Node of 'a Tree * 'a Tree

;;
let rec fmap f n = match n with
  | Leaf(a) -> Leaf(f a)
  | Node(t1,t2) -> Node(fmap f t1,fmap f t2)
;;
let p1 x = S(x)
;;
let main t = fmap p1 t







;;
