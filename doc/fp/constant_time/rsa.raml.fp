
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
 *   examples/constant_time/rsa.raml
 *
 * Author:
 *   Van Chan Ngo (2016)
 *
 * Description:
 *
 *)


(**
 * RSA encryption and decryption
 * e : encryption key
 * d : decryption key
 * m : plain message
 * c : cipher text
 * c = m^e mod n
 * m = c^d mod n
 *)
;;
;;

let rec pow res base exp n =
  match exp with
  | Nil -> res
  | Cons(h,t) ->
	 let unused =  in let next_base = base * base in
    if h then
      (
      let next_res = (res * base) mod n in
		pow next_res next_base t n)
    else
	   pow res next_base t n
;;
;;

let enc_rsa m e n = pow 1 m e n
;;
;;

let dec_rsa c d n = pow 1 c d n
;;
;;

let rec consume_pow res base exp n =
  match exp with
  | Nil -> res
  | Cons(h,t) ->
    let unused =  in let next_base = base * base in
    if h then
      (
      let next_res = (res * base) mod n in
	   consume_pow next_res next_base t n)
    else
		(consume t;
		consume_pow res next_base t n)
;;
;;

let c_enc_rsa m e n = consume_pow 1 m e n
;;
;;

let c_dec_rsa c d n = consume_pow 1 c d n
;;
