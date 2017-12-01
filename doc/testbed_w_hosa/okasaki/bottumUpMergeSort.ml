
type 'a list = Nil | Cons of 'a * 'a list;;
type Nat = 0 | S of Nat;;
type Bool = True | False;;
type 'a MergeSort = MS of Nat * 'a list;;

let rec leq x y = match x with
    | 0 -> True
    | S(x1) -> match y with
      | S(y1) -> leq x1 y1
      | 0 -> False
                 ;;

let rec mrg xs ys = match xs with
  | Nil -> ys
  | Cons(x,xs') ->
     match ys with
     | Nil -> xs
     | Cons(y,ys') -> match leq x y with
                      | True -> Cons(x,mrg xs' ys)
                      | False -> Cons(y, mrg xs ys')
;;


let empty = MS(0,Nil)
;;


let rec mod2 x = match x with
  | 0 -> 0
  | S(x) -> match x with
            | 0 -> S(0)
            | S(y) -> mod2 y
;;

let rec eq x y = match x with
  | 0 -> (match y with
         | 0 -> True
         | S(y) -> False)
  | S(x') -> match y with
             | 0 -> False
             | S(y') -> eq x' y'
;;

let rec div2 x = match x with
  | 0 -> 0
  | S(x) -> match x with
            | 0 -> 0
            | S(y) -> S(div2 y)
;;

let head xs = match xs with
  | Cons(x,xs') -> x
;;

let tail xs = match xs with
  | Cons(x,xs') -> xs'
;;

let rec addSeg seg segs size = match eq (mod2 size) 0 with
  | True -> Cons(seg,segs)
  | False -> addSeg (mrg seg (head segs)) (tail segs) (div2 size)
;;

let add x ys = match ys with
  | MS(size,segs) -> MS(S(size),addSeg Cons(x,Nil) segs size)
;;

let rec foldl f acc xs = match xs with
  | Nil -> acc
  | Cons(x,xs') -> foldl f (f acc x) xs'
;;

let rec sort xs = match xs with
  | MS(size,segs) -> foldl mrg Nil segs
;;
