type 'a option = None | Some of 'a
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type 'a Tree = Leaf of 'a | Node of 'a Tree * 'a Tree
;;
type 'a list = Nil | Cons of 'a * 'a list

;;
let rec dfsAcc g n acc = match n with
  | Leaf(x) -> g x acc
  | Node(t1, t2) -> dfsAcc g t2 (dfsAcc g t1 acc)
;;
let rec revApp l acc = match l with
  | Nil -> acc
  | Cons(y,ys) -> revApp ys Cons(y,acc)

;;
let c a b = Cons(a,b)
;;
let flatten t = revApp (dfsAcc c t Nil) Nil









;;
