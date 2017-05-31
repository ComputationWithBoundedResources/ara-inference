
type 'a list = Nil | Cons of 'a * 'a list;;
type Nat = 0 | S of Nat;;
type Weigth = Weight of Nat list;;
type Bool = True | False;;

let rec lt x y = match x with
    | 0 -> (match y with
           | 0 -> False
           | S(y1) -> True)
    | S(x1) -> (match y with
      | S(y1) -> lt x1 y1
      | 0 -> False)
                 ;;

let rec addNat x y = match x with
  | 0 -> y
  | S(x) -> S(addNat x y)
;;

let rec mult x y = match x with
  | 0 -> 0
  | S(x) -> addNat (mult x y) y
;;

let rec carry w xs = match xs with
  | Nil -> Cons(w,Nil)
  | Cons(w',ws') -> match lt w w' with
                    | True -> Cons(w,Cons(w',ws'))
                    | False -> carry (mult S(S(0)) w) ws'
;;

