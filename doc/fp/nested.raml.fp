
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

type ('a,'b,'c) l =
| A of 'a * ('a,'b,'c) l
| B of 'b * ('a,'b,'c) l
| T of 'c * ('a,'b,'c) l * ('a,'b,'c) l
| Nil

;;

let rec append l1 l2 =
  match l1 with
    | x::xs -> []
    | _ -> []

;;

let hello = []
;;

let world = []

;;

(* let hello' = append hello (A(3,Nil)) in *)
(* let world' = append world (B(4,Nil)) in *)
append hello world
