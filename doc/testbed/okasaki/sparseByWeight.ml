
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

let rec eq x y = match x with
  | 0 -> (match y with
         | 0 -> True
         | S(i) -> False)
  | S(x) -> match y with
            | 0 -> False
            | S(y) -> eq x y
;;

let rec borrow w ws = match ws with
  | Cons(w',ws') -> match eq w w' with
                    | True -> ws'
                    | False -> Cons(w, borrow (mult S(S(0)) w) ws)
;;

let inc ws = carry S(0) ws
;;

let dec ws = borrow S(0) ws
;;

let rec add ws1' ws2' = match ws2' with
  | Nil -> ws1'
  | Cons(w2,ws2) -> match ws1' with
                    | Nil -> ws2'
                    | Cons(w1,ws1) -> match lt w1 w2 with
                                      | True -> Cons(w1,add ws1 Cons(w2,ws2))
                                      | False -> match lt w2 w1 with
                                                 | True -> Cons(w2, add Cons(w1,ws1) ws2)
                                                 | False -> carry (mult S(S(0)) w1) (add ws1 ws2)
;;


