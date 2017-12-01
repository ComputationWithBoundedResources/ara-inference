
type 'a list = Nil | Cons of 'a * 'a list;;
type Digit = Zero | One;;
type Nat = Nat of Digit list;;


let rec inc xs = match xs with
  | Nil -> Cons(One,Nil)
  | Cons(d,ds) -> match d with
                  | Zero -> Cons(One,ds)
                  | One -> Cons(Zero,inc ds)
;;


let rec dec xs = match xs with
  | Cons(d,ds) -> match d with
                  | One -> (match ds with
                           | Nil -> Nil
                           | Cons(i,j) -> Cons(Zero,ds))
                  | Zero -> Cons(One,dec ds)
;;


let rec add ds1 ds2 = match ds2 with
  | Nil -> ds1
  | Cons(d2,dd2) -> match ds1 with
                    | Nil -> ds2
                    | Cons(d1,dd1) -> match d2 with
                                      | Zero -> Cons(d1,add dd1 dd2)
                                      | One -> match d1 with
                                               | Zero -> Cons(d2,add dd1 dd2)
                                               | One -> Cons(Zero,inc(add dd1 dd2))
;;
