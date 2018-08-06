let rec leqNat x y =
  match y with
  | 0 -> True
  | S(y') -> match x with
            | S(x') -> leqNat x' y'
            | 0 -> False
;;
let rec eqNat x y =
  match y with
  | 0 -> match x with
      | 0 -> True
      | S(x') -> False
  | S(y') -> match x with
            | S(x') -> eqNat x' y'
            | 0 -> False
;;
let rec geqNat x y =
  match x with
   | 0 -> False
   | S(x') -> match y with
              | 0 -> True
              | S(y') -> geqNat x' y'
;;
let rec ltNat x y =
  match y with
   | 0 -> False
   | S(y') -> match x with
        | 0 -> True
        | S(x') -> ltNat x' y'
;;
let rec gtNat x y =
  match x with
   | 0 -> False
   | S(x') -> match y with
             | 0 -> True
             | S(y') -> gtNat x' y'


;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
;;
let ite b th el = match b with
   | True()-> th
   | False()-> el
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
let rec mult n m = match n with
   | 0 -> 0
   | S(x) -> S(plus (mult x m) m)
;;

type ('a,'b,'c) triple = Triple of 'a * 'b * 'c

;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)
;;
type bool = True | False
;;
type 'a option = None | Some of 'a
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type Exception = Out_of_Bounds
;;
type ('a,'b) pair = Pair of 'a * 'b

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Cases  * *
 *
 * File:
 *   examples/echolon_form.raml
 *
 * Author:
 *   Ankush Das (S(S(0))015)
 *
 * Description:
 *   Bring a matrix into echolon form.
 *)

;;
let rec size l =
	match l with
	| Nil()-> 0
	| Cons(x,xs) -> S(size xs)

;;
let rec get_elem l i =
	match l with
	| Nil()-> error Out_of_Bounds
	| Cons(x,xs) ->
		ite (eqNat i 0) x (get_elem xs (minus i S(0)))
;;
let rec get_2elems l1 l2 i =
	match l1 with
	| Nil()-> error Out_of_Bounds
	| Cons(x1,x1s) ->
		match l2 with
		| Nil()-> error Out_of_Bounds
		| Cons(x2,x2s) ->
			ite (eqNat i 0) Pair(x1, x2) (get_2elems x1s x2s (minus i S(0)))
;;
let rec subtract_row_helper r1 r2 q =
	match r1 with
	| Nil()-> Nil
	| Cons(v1,v1s) ->
		match r2 with
		| Nil()-> Nil
		| Cons(v2,v2s) ->
			Cons(minus v2 (mult v1 q),subtract_row_helper v1s v2s q)
;;
let subtract_row r1 r2 i =
	match get_2elems r1 r2 i with
  | Pair(f1,f2) -> match div_mod f2 f1 with
                   | Triple(q,m,u2) -> subtract_row_helper r1 r2 q
;;
(* let rec subtract_helper m row i =
 * 	match m with
 * 	| Nil()-> Nil
 * 	| Cons(r,rs) -> Cons(subtract_row row r i,subtract_helper rs row i)
 * ;;
 * let rec concat l1 l2 =
 * 	match l1 with
 * 	| Nil()-> l2
 * 	| Cons(x,xs) -> Cons(x,concat xs l2)
 * ;;
 * let rec tl l i =
 * 	ite (eqNat i 0) l
 * 		(match l with
 * 		| Nil()-> error Out_of_Bounds
 * 		| Cons(x,xs) -> (tl xs (minus i S(0))))
 * ;;
 * let rec hd_helper l i acc =
 * 	ite (eqNat i 0) acc
 * 		(match l with
 * 		| Nil()-> error Out_of_Bounds
 * 		| Cons(x,xs) -> (hd_helper xs (minus i S(0)) (Cons(x,acc))))
 * ;;
 * let rec reverse_helper l acc =
 * 	match l with
 * 	| Nil()-> acc
 * 	| Cons(x,xs) -> reverse_helper xs (Cons(x,acc))
 * ;;
 * let reverse l =
 * 	reverse_helper l Nil
 * ;;
 * let hd l i =
 * 	reverse (hd_helper l i Nil)
 * ;;
 * let rec split_helper l i j hd =
 * 	ite (eqNat i j) Pair(reverse hd, l)
 * 		(match l with
 * 		| Nil()-> error Out_of_Bounds
 * 		| Cons(x,xs) -> split_helper xs i S(j) (Cons(x,hd)))
 * ;;
 * let split l i =
 * 	split_helper l i 0 Nil
 * ;;
 * let subtract m i =
 * 	let row = get_elem m i in
 * 	match split m (S(i)) with
 *   | Pair(head,tail) -> concat head (subtract_helper tail row i)
 * ;;
 * let rec echelon_helper_old m i =
 * 	ite (eqNat i (size m)) m (echelon_helper_old (subtract m i) S(i))
 * ;;
 * let echelon_form_old m =
 * 	echelon_helper_old m 0
 * ;;
 * let rec echelon_helper m i sizem =
 * 	match sizem with
 * 	| Nil()-> m
 * 	| Cons(r,rs) -> echelon_helper (subtract m i) S(i) rs
 * ;;
 * let echelon_form m =
 * 	echelon_helper m 0 m
 * ;; *)


(* let r1 = S(0).Cons(,2).Cons(,3).::Nil
 * let r2 = S(S(S(S(0)))).Cons(,5).Cons(,6).::Nil
 * let r3 = 7.Cons(,8).Cons(,10).::Nil
 * ;;
 * let m = (Cons(r1,Cons(r2,r3)))::Nil
 *
 * echelon_form m
 * ;; *)
