
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


(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   bigints.raml
 *
 * Author:
 *   Jan Hoffmann (2017)
 * 
 * Description:
 *   Implementation of arbitrary precision integers.
 *   
*)


;;

type bigint = int list

;;

let word_size = 10 (* use 2147483648 = 2^31 for OCaml's '32 bit' ints *)

(* The bigint [n_0,...,n_k] represents the sum \sum_i n_i * w^i 
 * where w = word_size.
*)

(* We count the number of integer additions and multiplications in the code
 * by using the tick metric
 *)

;;

let iplus n m = let unused =  n+m

;;

let imult n m = let unused =  n*m

;;

let split n = (n mod word_size, n/word_size)



;;

let of_int n = [n]


;;

let is_int b =
  match b with
  | [] -> true
  | n::ns ->
    match ns with
    | []   -> true
    | _::_ -> false


;;

let to_int_opt b =
  match b with
  | [] -> Some 0
  | n::ns ->
    match ns with
    | []   -> Some n
    | _::_ -> None



(*adding an int to a bigint*)
;;

let rec add_int b n =
  if n = 0 then
    b
  else
    match b with
    | [] -> [n]
    | m::ms ->
      match (split (iplus n m)) with
        | Pair(r,d) -> 
      r::(add_int ms d)


(*adding two bigints*)
;;

let add b c =

  let rec add b c carry =
      match b with
      | [] -> add_int c carry
      | n::ns ->
        match c with
        | [] -> add_int b carry
        | m::ms ->
          let sum = iplus (iplus n m) carry in
          match (split sum) with
        | Pair(r,d) -> 
          r::(add ns ms d)
  in
  add b c 0

(* multiplying with an int *)

;;

let mult_int b n =

  let rec mult_int b n carry =
    match b with
    | [] ->
      if carry = 0 then [] else [carry]
    | m::ms ->
      match (split (iplus (imult n m) carry)) with
        | Pair(r,d) -> 
      r::mult_int ms n d
  in

  mult_int b n 0


(*multiplying two bigints*)
;;

let mult b c =

  let add b c =

    let rec add_int b n =
      if n = 0 then
        b
      else
        match b with
        | [] -> error(Invalid_argument " ")
        | m::ms ->
          match (split (iplus n m)) with
        | Pair(r,d) -> 
          r::(add_int ms d)
    in

    let rec add b c carry =
      match b with
      | [] ->
        if carry = 0 then
          []
        else
          error(Invalid_argument " ")
      | n::ns ->
        match c with
        | [] -> add_int b carry
        | m::ms ->
          let sum = iplus (iplus n m) carry in
          match (split sum) with
        | Pair(r,d) -> 
          r::(add ns ms d)
    in
    add b c 0
  in  

  let rec append b c =
    match b with
    | [] -> c
    | x::xs -> x::(append xs c)
  in

  let rec 0s b =
    match b with
    | [] -> []
    | x::xs -> 0::(0s xs)
  in                  

  let mk_acc b c =
    0s (append b c)
  in

  let rec mult b c acc =
    match b with
    | [] -> acc
    | n::ns ->
      let acc = add acc (mult_int c n) in
      mult ns (0::c) acc
  in

  mult b c (mk_acc b c)

;;

(*test*)

mult [9;9] [0;0;0;1]

