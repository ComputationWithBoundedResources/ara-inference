
type nat = 0 | Cons of nat;;
type 'a list = Nil | Cons of 'a * 'a list;;
type 'a listL = NilL | ConsL of 'a * 'a listL;;

let rec take_l n xs =
  match force xs with
  | NilL -> Nil
  | ConsL(x,xs') ->
     match n with
     | 0 -> Nil
     | S(n') -> Cons(x,take_l n' xs')
;;

let rec zeros = lazy ConsL(0, zeros)
;;

let main n = take_l n zeros;;
