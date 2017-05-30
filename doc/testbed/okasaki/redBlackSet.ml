
type Bool = True | False;;
type Nat = 0 | S of Nat;;
type Elem = Elem of Nat;;
type Color = R | B;;
type Tree = E | T of Color * Tree * Elem * Tree;;
type Set = Tree;;

let empty = E
;;

let rec lt x y = match x with
    | 0 -> (match y with
           | 0 -> False
           | S(y1) -> True)
    | S(x1) -> (match y with
      | S(y1) -> lt x1 y1
      | 0 -> False)
                 ;;

let rec ltElem xe ye = match xe with
  | Elem(x) -> match ye with
  | Elem(y) -> lt x y
                   ;;


let rec member x tree = match tree with
 | E -> False
 | T(c,a,y,b) -> match ltElem x y with
 | True -> member x a
 | False -> match ltElem y x with
 | True -> member x b
 | False -> True
;;

let balance xs = match xs with
     | Tree(c,t1,z,d) -> match c with
       | B -> match a with
         | Tree(c2,t2,y,c) -> match c2 with
           | R -> match t2 with
             | Tree(c3,b,y,c) -> match c3 with
               | R -> Tree(R,Tree(B,a,x,b),y,Tree(B,c,z,d))
               |
;;
