
type Nat = 0 | S of Nat;;
type Elem = Elem of Nat | ErrorElem;;
type Heap = E | T of Heap * Elem * Heap | ErrorHeap;;
type Bool = True | False;;
type ('a,'b) Pair = Pair of 'a * 'b;;

let empty = E
;;

let isEmpty h = match h with
  | E -> True
  | T(h1,e,h2) -> False
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

let rec partition pivot heap = match heap with
  | E -> Pair(E,E)
  | T(a,x,b) -> match leqElem x pivot with
    | True -> (match b with
      | E -> Pair(T(a,x,b),E)
      | T(b1,y,b2) -> (match leqElem y pivot with
        | True -> (match partition pivot b2 with
          | Pair(small,big) -> Pair(T(T(a,x,b1),y,small),big))
        | False -> (match partition pivot b1 with
          | Pair(small,big) -> Pair(T(a,x,small),T(big,y,b2)))))
    | False -> match a with
      | E -> Pair(E,T(a,x,b))
      | T(a1,y,a2) -> match leqElem y pivot with
        | True -> (match partition pivot a2 with
           | Pair(small,big) -> Pair(T(a1,y,small),T(big,x,b)))
        | False -> (match partition pivot a1 with
           | Pair(small,big) -> Pair(small,T(big,y,T(a2,x,b))))
           ;;

