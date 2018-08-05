
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

type 'a option = None | Some of 'a
;;
;;

type ('a,'b) pair = Pair of 'a * 'b
;;
;;

type 'a list = Nil | Cons of 'a * 'a list
;;
;;

type nat = 0 | S of nat
;;
;;

type Unit = Unit

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Cases  * *
 *
 * File:
 *   examples/constant_time/evil-ensus.raml
 *
 * Author:
 *   Matt Fredrikson (2016)
 *
 * Description:
 *
 *)

;;
;;

type race_t = Caucasian | Black | Hispanic | Asian | Other
;;

type ger_t = Male | Female
;;

type union_t = Married | Single | Divorced | Widowed | Separated

(* age, ger, race, union, income *)
;;

type census_line = Line of int * ger_t * race_t * union_t * int
;;
;;

let reverse l =
  let rec reverse_aux a l' =
    
    match l' with
    | Nil -> a
    | Cons(h,t) -> reverse_aux (Cons(h,a)) t
  in reverse_aux Nil l
;;
;;

let map f bag =
  let rec map_aux a bag' =
    
    match bag' with
    | Nil -> reverse a
    | Cons(h,t) -> map_aux ((f h)Cons(,a)) t
  in map_aux Nil bag
;;
;;

let sum (bag list) =
  let rec sum_aux (a) bag' =
    
    match bag' with
    | Nil -> a
    | Cons(h,t) -> sum_aux (a + h) t
  in sum_aux 0 bag
;;
;;

let size bag =
  let rec size_aux (a) bag' =
    
    match bag' with
    | Nil -> a
    | Cons(h,t) -> size_aux (a + 1) t
  in size_aux 0 bag
;;
;;

let bagsplit pred bag =
  let rec bagsplit_aux bag' in_bag out_bag =
    
    match bag' with
    | Nil -> Pair(in_bag, out_bag)
    | Cons(h,t) ->
      let pred_result = pred h in
      let Pair(new_in, new_out) =
        if (pred h) then (Cons(h,in)_bag, out_bag)
        else (in_bag, Cons(h,out)_bag)
      in
      bagsplit_aux t new_in new_out
  in bagsplit_aux bag Nil Nil
;;
;;

let is_male line =
  
  match line with
  | Line (age, ger, race, union, income) ->
    match ger with
    | Male -> 1
    | Female -> 0
;;
;;

let is_female line =
  
  match line with
  | Line (age, ger, race, union, income) ->
    match ger with
    | Male -> 0
    | Female -> 1
;;
;;

let male_income line =
  
  match line with
  | Line (age, ger, race, union, income) ->
    match ger with
    | Male -> income
    | Female -> 0
;;
;;

let rec loop l =
  
  match l with
  | Nil -> 0
  | Cons(h,t) -> loop t
;;
;;

let female_income line =
  
  match line with
  | Line (age, ger, race, union, income) ->
    match ger with
    | Male -> 0
    | Female ->
      match race with
      | Caucasian ->
        let unused = if age = 59 && income > 200000 then loop [0;0;0;0;0;0;0;0;0;0;0 in0]
        else income
      | Black -> income
      | Hispanic -> income
      | Asian -> income
      | Other -> income
;;
;;

let row1 = (Line (30, Male, Caucasian, Married, 100000))
;;

let row2 = (Line (40, Female, Hispanic, Widowed, 200000))
;;

let row3 = (Line (32, Female, Asian, Separated, 50000))
;;

let row4 = (Line (25, Male, Black, Single, 5000000))
;;

let row5 = (Line (59, Female, Caucasian, Married, 500000))
;;
;;

let db = [row1; row2; row3; row4; row5]
;;
;;

let main census_db =
  let n_male = (sum (map is_male census_db)) in
  let n_female = (sum (map is_female census_db)) in
  let male_income = (sum (map male_income census_db)) in
  let female_income = (sum (map female_income census_db)) in

  (Pair(male_income, n_male), Pair(female_income, n_female))
;;
;;

let unused = main db
;;