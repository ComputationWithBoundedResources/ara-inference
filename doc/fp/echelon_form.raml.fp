
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
 *   examples/echolon_form.raml
 *
 * Author:
 *   Ankush Das (2015)
 * 
 * Description:
 *   Bring a matrix into echolon form.
 *)


;;

let rec size l =
	match l with
	| [] -> 0
	| x::xs -> (tick(1.0); 1 + (size xs));;

exception Out_of_Bounds

;;

let rec get_elem l i = 
	match l with
	| [] -> error
	| x::xs ->
		if (i = 0) then x
		else (tick(1.0); get_elem xs (i-1));;

;;

let rec get_2elems l1 l2 i =
	match l1 with
	| [] -> error
	| x1::x1s ->
		match l2 with
		| [] -> error
		| x2::x2s ->
			if (i = 0) then (x1, x2)
			else (tick(1.0); get_2elems x1s x2s (i-1));;

;;

let rec subtract_row_helper r1 r2 q =
	match r1 with
	| [] -> []
	| v1::v1s ->
		match r2 with
		| [] -> []
		| v2::v2s -> 
			(tick(1.0); (v2 -. v1*.q)::(subtract_row_helper v1s v2s q));;

;;

let subtract_row r1 r2 i =
	match (get_2elems r1 r2 i) with
        | Pair(f1, f2) -> 
	let q = f2 /. f1 in
	(subtract_row_helper r1 r2 q);; 

;;

let rec subtract_helper m row i =
	match m with
	| [] -> []
	| r::rs -> (tick(1.0); subtract_row row r i::subtract_helper rs row i);;

;;

let rec concat l1 l2 =
	match l1 with
	| [] -> l2
	| x::xs -> (tick(1.0); x::concat xs l2);;

;;

let rec tl l i =
	if (i = 0) then l
	else
		match l with
		| [] -> error
		| x::xs -> (tick(1.0); tl xs (i-1));;

;;

let rec hd_helper l i acc =
	if (i = 0) then acc
	else
		match l with
		| [] -> error
		| x::xs -> (tick(1.0); hd_helper xs (i-1) (x::acc));;

;;

let rec reverse_helper l acc =
	match l with
	| [] -> acc
	| x::xs -> (tick(1.0); reverse_helper xs (x::acc));;

;;

let reverse l =
	reverse_helper l [];;

;;

let hd l i =
	reverse (hd_helper l i []);;

;;

let rec split_helper l i j hd =
	if (i = j) then (reverse hd, l)
	else
		match l with
		| [] -> error
		| x::xs -> (tick(1.0); split_helper xs i (j+1) (x::hd));;

;;

let split l i =
	split_helper l i 0 [];;

;;

let subtract m i =
	let row = get_elem m i in
	match (split m (i+1)) with
        | Pair(head, tail) -> 
	(tick(1.0); concat head (subtract_helper tail row i));;

;;

let rec echelon_helper_old m i =
	if (i = (size m)) then m
	else echelon_helper_old (subtract m i) (i+1);;

;;

let echelon_form_old m =
	echelon_helper_old m 0;;

;;

let rec echelon_helper m i sizem =
	match sizem with
	| [] -> m
	| r::rs -> echelon_helper (subtract m i) (i+1) rs;; 

;;

let echelon_form m =
	echelon_helper m 0 m;;

;;

let r1 = 1.::2.::3.::[];;
;;

let r2 = 4.::5.::6.::[];;
;;

let r3 = 7.::8.::10.::[];;

;;

let m = r1::r2::r3::[];;

echelon_form m
