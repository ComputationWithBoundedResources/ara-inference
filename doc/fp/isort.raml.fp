
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

let rec compare_list (l1:int list) l2 =
  match l1 with
  | [] -> true
  | x::xs ->
     match l2 with
     | [] -> false
     | y::ys ->
	if x = y then
	  compare_list xs ys
	else
	  x < y


;;

let rec insert le x l =
  match l with
  | [] -> [x]
  | y::ys ->
     if le y x then y::insert le x ys
     else x::y::ys

		  
;;

let rec isort le l =
  match l with
  | [] -> []
  | x::xs -> insert le x (isort le xs)

;;

let isort_list = isort compare_list

;;

let l1 = [1;2;3;4;5]
;;

let l2 = [1;2;3;4;4]
;;

let l3 = [1;2;3;4]
;;

let l4 = [1;2;3;4;4;4]
;;

let l5 = [1;1;3;4;4;4]
;;

let l6 = [-1;0;1;2;3;4]
;;

let l7 = [1;2;3;5;5]
;;

let l8 = []
	   
;;

let _ = isort_list [l1;l2;l3;l4;l5;l6;l7;l8]
