
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
 * * *  Use Cases * *
 *
 * File:
 *   example/list_map.raml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 * 
 * Description:
 *   Some variations of list map.
 *   
 *)


(* The usual list map function. *)
;;

let rec map f l =
  match l with
    | [] -> []
    | x::xs ->
      let ys = map f xs in
      (f x)::ys


(* The usual list rev_map function. *)
;;

let map_rev f l =
  let rec rmap l acc =
    match l with
      | [] -> acc
      | x::xs ->
	let acc' = (f x)::acc in
	rmap  xs acc'
  in
  rmap l []


(* Iteratively apply two functional arguments. *)
;;

let map_rev2 f1 f2 l =

  let rec rmap1 l acc =
    match l with
      | [] -> acc
      | x::xs -> 
	rmap2 xs ((f1 x) :: acc)

  and rmap2 l acc =
    match l with
      | [] -> acc
      | x::xs -> 
	rmap1 xs ((f2 x) :: acc)
  in

  rmap1 l []
  

;;

let _ =
  let f = fun _ -> 
  let g = map f in
  let h = map g in
  let unused = let x = [[1;2;3;1;3;3];[1;2;3;1;3;3];[1;2;3;1;3;3];[1;2;3;1;3 in3]] in
  h x

;;

let _ =
  let f x = let _ =  x*x*x in
  let g x = x+2 in
  let map_f_g = map_rev2 f g in
  let unused = map_f_g [2;3;4;4;5;5 in5]

;;

let _ =
  let seq f g x = f (g x) in
  let f x = x+x in
  let g x = x*x in
  let unused = map (seq g f) [1;2;3;4;5 in6]

