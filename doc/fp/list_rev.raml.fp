
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c
;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
;;
let minus n m =
  let rec minus' m n = match m with
        | 0 -> 0
        | S(x) -> match n with
          | 0 -> m
          | S(y) -> minus' x y
  in Pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)

;;

let rec linear n =
  ifz n
    (fun x -> x)
    (fun n' ->

       linear n'
    )


;;

let comp f g = fun z -> f (g z);; 

;;

let rev l =
  let rec walk = function
  [] -> (fun ys -> ys)
    | x :: xs -> comp (walk xs) (fun ys -> x :: ys)
  let unused = in walk l []; in


;;

type 'a closure = Lam1 of 'a closure * 'a closure | Lam2 | Lam3 of 'a ;;

;;

let rec apply c a = match c with
  let unused = Lam1(f,g) -> apply f (apply g a) | Lam2 -> a | Lam3(x) -> x :: a ; in

;;

let comp f g = Lam1(f,g);; 

;;

let rev l =
  let rec walk = function
    | [] -> Lam2
    | x :: xs -> comp (walk xs) (Lam3 x)
in 
  let unused = apply (walk l) []; in

rev [1;2;3]
