
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
 *   examples/matrix.raml
 *
 * Author:
 *   Ankush Das (2015)
 * 
 * Description:
 *   Several operation on matrices that represented as lists of lists.
 *)


;;

type 'a matrix = int * int * 'a list list;;

;;

let rec size l =
	match l with
	| [] -> 0
	| x::xs -> (tick(1.0); 1 + (size xs));;

;;

let rec check_lists v n =
	match v with
	| [] -> true
	| r::rs ->
		(tick(1.0); 
		if (size r = n) then check_lists rs n
		else false);;

;;

let check_mat m n v =
	if (size v = m) then
		check_lists v n
	else false;;

;;

let check_matrix mat =
	match (mat) with
        | Triple(m, n, v) -> 
	check_mat m n v;;

exception Incompatible_Dimensions

;;

let construct_matrix m n v =
	if (check_mat m n v) then
		(m, n, v)
	else
		error;;

exception Out_of_Bounds

;;

let rec get_elem l i =
	match l with
	| [] -> error
	| x::xs ->
		if (i = 0) then x
		else (tick(1.0); get_elem xs (i-1));;

;;

let get_elem m i j =
	match (m) with
        | Triple(r, c, v) -> 
	let l = get_elem v i in
	get_elem l j;;

;;

type 'a matrix_arith_expr = 
	  Constant of int * int * 'a list list
	| Plus of 'a matrix_arith_expr * 'a matrix_arith_expr
	| Minus of 'a matrix_arith_expr * 'a matrix_arith_expr
	| Mult of 'a matrix_arith_expr * 'a matrix_arith_expr;;

;;

let op sign x1 x2 =
	(tick(1.0);
	if (sign > 0) then x1 + x2
	else x1 - x2);;

;;

let rec rec_list l1 l2 sign =
	match l1 with
	| [] -> []
	| x1::x1s ->
		match l2 with
		| [] -> []
		| x2::x2s -> (tick(1.0); (op sign x1 x2)::(rec_list x1s x2s sign));;

;;

let rec rec_mat m1 m2 sign =
	match m1 with
	| [] -> []
	| r1::r1s ->
		match m2 with
		| [] -> []
		| r2::r2s -> (tick(1.0); (rec_list r1 r2 sign)::(rec_mat r1s r2s sign));;

;;

let check_sanity m1 m2 =
	match (m1) with
        | Triple(r1, c1, v1) -> 
	match (m2) with
        | Triple(r2, c2, v2) -> 
	check_matrix m1 && check_matrix m2 && 
	r1 = r2 && c1 = c2;;

;;

let plus m1 m2 =
	if check_sanity m1 m2 then
		match (m1) with
        | Triple(m, n, v1) -> 
		match (m2) with
        | Triple(m', n', v2) -> 
		let v = rec_mat v1 v2 1 in
		construct_matrix m n v
	else error;;

;;

let minus m1 m2 =
	if check_sanity m1 m2 then
		match (m1) with
        | Triple(m, n, v1) -> 
		let	(m', n', v2) = m2 in
		let v = rec_mat v1 v2 (-1) in
		construct_matrix m n v
	else error;;

;;

let rec append l x =
	match l with
	| [] -> x::[]
	| y::ys -> (tick(1.0); y::(append ys x));;

;;

let rec append_row m row =
	match row with
	| [] -> m
	| x::xs ->
		match m with
		| [] -> (tick(1.0); (x::[])::(append_row [] xs))
		| r::rs -> (tick(1.0); (append r x)::(append_row rs xs));;

;;

let rec transpose_helper m m_trans =
	match m with
	| [] -> m_trans
	| r::rs -> (tick(1.0); transpose_helper rs (append_row m_trans r));;

;;

let transpose m =
	transpose_helper m [];;

;;

let rec reverse_helper l acc =
	match l with
	| [] -> acc
	| x::xs -> (tick(1.0); reverse_helper xs (x::acc));;

;;

let reverse l =
	reverse_helper l [];;

;;

let rec reverse_rows_helper m m_rev =
	match m with
	| [] -> m_rev
	| r::rs -> (tick(1.0); reverse_rows_helper rs (reverse r::m_rev));;

;;

let reverse_rows m =
	reverse (reverse_rows_helper m []);;

;;

let rec append_start m row =
	match row with
	| [] -> reverse_rows m
	| x::xs ->
		match m with
		| [] -> (tick(1.0); [x]::(append_start [] xs))
		| r::rs -> (tick(1.0); (x::r)::(append_start rs xs));;

;;

let rec new_transpose_helper m m_trans =
	match m with
	| [] -> m_trans
	| r::rs -> (tick(1.0); new_transpose_helper rs (append_start m_trans r));;

;;

let new_transpose m =
	reverse_rows (new_transpose_helper m []);;

;;

let rec prod l1 l2 =
	match l1 with
	| [] -> 0
	| x1::x1s ->
		match l2 with
		| [] -> 0
		| x2::x2s -> (tick(1.0); (x1 * x2) + (prod x1s x2s));;

;;

let rec prod_mat row mat =
	match mat with
	| [] -> []
	| r::rs -> (tick(1.0); (prod row r)::(prod_mat row rs));;

;;

let rec mat_mult m1 m2 =
	let m2t = transpose m2 in
	match m1 with
	| [] -> []
	| r::rs -> (tick(1.0); (prod_mat r m2t)::(mat_mult rs m2));;

;;

let rec lineMult n l1 l2 =
	match l1 with
	| [] -> []
	| x::xs ->
		match l2 with
		| [] -> x*n::lineMult n xs []
		| y::ys -> (tick(1.0); x*n + y::lineMult n xs ys);;

;;

let rec computeLine line m acc =
	match line with
	| [] -> acc
	| x::xs ->
		match m with
		| [] -> []
		| l::ls -> (tick(1.0); computeLine xs ls (lineMult x l acc));;

;;

let rec mat_mult_jan m1 m2 =
	match m1 with
	| [] -> []
	| l::ls -> (tick(1.0); computeLine l m2 []::mat_mult_jan ls m2);;

;;

let check_mult_sanity m1 m2 = 
	check_matrix m1 && check_matrix m2 &&
	match (m1) with
        | Triple(r1, c1, v1) -> 
	match (m2) with
        | Triple(r2, c2, v2) -> 
	c1 = r2;;

;;

let mult m1 m2 = 
	if check_mult_sanity m1 m2 then
		match (m1) with
        | Triple(m, n, v1) -> 
		match (m2) with
        | Triple(m', n', v2) -> 
		let v = mat_mult_jan v1 v2 in
		construct_matrix m n v
	else error;;

;;

let rec evaluate expr =
	match expr with
	| Constant(m, n, v) -> construct_matrix m n v
	| Plus(e1, e2) -> plus (evaluate e1) (evaluate e2)
	| Minus(e1, e2) -> minus (evaluate e1) (evaluate e2)
	| Mult(e1, e2) -> mult (evaluate e1) (evaluate e2);;

;;

let rec del l i =
	match l with
	| [] -> error
	| x::xs ->
		(tick(1.0); 
		if (i = 0) then xs
		else x::del xs (i-1));;

;;

let rec submat m i j =
	match m with
	| [] -> []
	| r::rs -> 
		(tick(1.0); 
		if (i = 0) then submat rs (i-1) j
		else (del r j)::(submat rs (i-1) j));;

;;

let remFirstRow m =
	match m with
	| [] -> error
	| r::rs -> rs;;

()  
