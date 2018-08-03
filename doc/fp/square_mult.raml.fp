
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
 *   examples/square_mult.raml
 *
 * Author:
 *   Jan Hoffmann (2015)
 * 
 * Description:
 *   Squared exponentiation.
 *   
 *)

exception Invalid_argument 

;;

type binary = Zero | One


;;

let square_mult m k =
(*computes m^k*)
  
  let x = ref m in

  let rec sq_mult k =
    match k with
      | [] -> ()
      | b::bs ->
        let _ =
	  let _ = 
	  x := (!x)*(!x)
	in
	let _ =
	  match b with
	    | Zero -> ()
	    | One ->
	      let _ = tick(1.0) in
	      x := (!x)*m
	in
	sq_mult bs
  in
  match k with
    | [] -> error
    | b::bs ->
      match b with
	| Zero -> error
	| One ->
	  let _ = sq_mult bs in
	  !x

;;

square_mult 3 [One;Zero;One;One;Zero;Zero]
