
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
 * * *  Use Cases  * *
 *
 * File:
 *   examples/hello_world.raml
 *
 * Author:
 *   Jan Hoffmann (2015)
 * 
 * Description:
 *   Hello world with ASCII.
 *)


;;

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | x::xs ->
      let _ = tick(1.2) in
      x::(append xs l2)

;;

let hello = [72;101;108;108;111]
;;

let world = [87;111;114;108;100]

;;

;;

let hello' = append hello [32] in
;;

let world' = append world [33] in
append hello' world'

