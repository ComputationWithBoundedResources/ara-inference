
type 'a list = Nil | Cons of 'a * 'a list;;
type Bool = True | False;;
type Nat = 0 | S of Nat;;
type Elem = Elem of Nat;;
type Tree = Node of Nat * Elem * Tree list;;

let isEmtpy ls =
    match ls with
  | Nil -> True
  | Cons(x,xs) -> False
;;

let rank n =
    match n with
  | Node (r,x,c) -> r
;;

let root n =
    match n with
  | Node (r,x,c) -> x
;;

let rec lt x y = match x with
    | 0 -> (match y with
           | 0 -> False
           | S(y1) -> True)
    | S(x1) -> (match y with
      | S(y1) -> lt x1 y1
      | 0 -> False)
                 ;;

let rec leq x y = match x with
    | 0 -> True
    | S(x1) -> match y with
      | S(y1) -> leq x1 y1
      | 0 -> False
                 ;;

let rec leqElem xe ye = match xe with
  | Elem(x) -> match ye with
  | Elem(y) -> leq x y
                   ;;


let link t1 t2 =
    match t1 with
  | Node (r1,x1,c1) -> match t2 with
  | Node (r2,x2,c2) -> match leqElem x1 x2 with
    | True -> Node(S(r1),x1,Cons(t2,c1))
    | False -> Node(S(r1),x2,Cons(t1,c2))
             ;;

let rec insTree t xs = match xs with
  | Nil -> Cons(t,Nil)
  | Cons(t',ts') -> match leq (S(rank t)) (rank t') with
    | True -> Cons(t,Cons(t',ts'))
    | False -> insTree (link t t') ts'
                       ;;

let insert x ts = insTree (Node(0,x,Nil)) ts
                              ;;


let rec merge ts1 ts2 = match ts1 with
  | Nil -> ts2
  | Cons(t1,ts1') -> match ts2 with
  | Nil -> ts1
  | Cons(t2,ts2') -> match leq S(rank t1) (rank t2) with
  | True -> Cons(t1,(merge ts1' ts2))
  | False -> match leq S(rank t2) (rank t1) with
  | True -> Cons(t2,(merge ts1 ts2'))
  | False  -> insTree (link t1 t2) (merge ts1' ts2')
                     ;;


