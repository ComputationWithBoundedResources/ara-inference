
let rec leqNat x y =
  match y with
  | 0 -> True
  | S(y') -> (match x with
            | S(x') -> leqNat x' y'
            | 0 -> False)
;;
let rec eqNat x y =
  match y with
  | 0 -> (match x with
      | 0 -> True
      | S(x') -> False)
  | S(y') -> (match x with
            | S(x') -> eqNat x' y'
            | 0 -> False)
;;
let rec geqNat x y =
  match x with
   | 0 -> False
   | S(x') -> (match y with
              | 0 -> True
              | S(y') -> geqNat x' y')
;;
let rec ltNat x y =
  match y with
   | 0 -> False
   | S(y') -> (match x with
        | 0 -> True
        | S(x') -> ltNat x' y')
;;
let rec gtNat x y =
  match x with
   | 0 -> False
   | S(x') -> (match y with
             | 0 -> True
             | S(y') -> gtNat x' y')


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
        | S(x) -> (match n with
          | 0 -> m
          | S(y) -> minus' x y)
  in Pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c
;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> (match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> (match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)))
;;
let rec mult n m = match n with
   | 0 -> 0
   | S(x) -> S(plus (mult x m) m)
;;
type bool = True | False
;;
type 'a option = None | Some of 'a
;;
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
let land a b = match a with
           | False -> False
           | True -> b


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
 *   Ankush Das (S(S(0))015)
 *
 * Description:
 *   Several operation on matrices that represented as lists of lists.
 *)
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type 'a matrix = Matrix of nat * nat * ('a list) list
;;
let rec size l =
	match l with
	| Nil()-> 0
	| Cons(x,xs) -> plus S(0) (size xs)
;;
let rec check_lists v n =
	match v with
	| Nil()-> True
	| Cons(r,rs) -> ite (eqNat (size r) n) (check_lists rs n) False
;;
let check_mat m n v =
	ite (eqNat (size v) m) (check_lists v n) False
;;
let check_matrix mat = match mat with
  | Triple(m,n,v) -> check_mat m n v
;;
type Exception = Incompatible_Dimensions | Out_of_Bounds
;;
let construct_matrix m n v = ite ((check_mat m n v)) Triple(m, n, v) (error Incompatible_Dimensions)

;;
let rec get_elem l i =
	match l with
	| Nil()-> error Out_of_Bounds
	| Cons(x,xs) ->
		ite (eqNat i 0) x (get_elem xs (minus i S(0)))
;;
let get_elem m i j = match m with
  | Triple(r,c,v) -> let l = get_elem v i in
	                   get_elem l j
;;
type 'a matrix_arith_expr =
	  Constant of nat * nat * ('a list) list
	| Plus of 'a matrix_arith_expr * 'a matrix_arith_expr
	| Minus of 'a matrix_arith_expr * 'a matrix_arith_expr
	| Mult of 'a matrix_arith_expr * 'a matrix_arith_expr
;;
let op sign x1 x2 =
	ite (gtNat sign 0) (plus x1 x2) (minus x1 x2)
;;
let rec rec_list l1 l2 sign =
	match l1 with
	| Nil()-> Nil
	| Cons(x1,x1s) ->
		(match l2 with
		| Nil()-> Nil
		| Cons(x2,x2s) -> Cons(op sign x1 x2,rec_list x1s x2s sign))
;;
let rec rec_mat m1 m2 sign =
	match m1 with
	| Nil()-> Nil
	| Cons(r1,r1s) ->
		(match m2 with
		| Nil()-> Nil
		| Cons(r2,r2s) -> Cons(rec_list r1 r2 sign,rec_mat r1s r2s sign))
;;
let check_sanity m1 m2 =
  match m1 with
  | Triple(r1,c1,v1) -> (match m2 with
                         | Triple(r2,c2,c2) ->
                            land (land (land (check_matrix m1) (check_matrix m2))
	                            (eqNat r1 r2)) (eqNat c1 c2)
                        )

;;
let plus m1 m2 =
	ite (check_sanity m1 m2)
    (match m1 with
     | Triple (m,n,v1) ->
        (match m2 with
         | Triple(m',n',v2) ->
		        let v = rec_mat v1 v2 S(0) in
		        construct_matrix m n v))
	  (error Incompatible_Dimensions)
;;
let minus m1 m2 =
	ite (check_sanity m1 m2)
  (match m1 with
     | Triple (m,n,v1) ->
        (match m2 with
         | Triple(m',n',v2) ->
		        let v = rec_mat v1 v2 (S(0)) in
		        construct_matrix m n v))
	(error Incompatible_Dimensions)
;;
let rec append l x =
	match l with
	| Nil()-> Cons(x,Nil)
	| Cons(y,ys) -> Cons(y,append ys x)
;;
let rec append_row m row =
	match row with
	| Nil()-> m
	| Cons(x,xs) ->
		match m with
		| Nil()-> Cons(Cons(x,Nil),append_row Nil xs)
		| Cons(r,rs) -> Cons(append r x,append_row rs xs)
;;
let rec transpose_helper m m_trans =
	match m with
	| Nil()-> m_trans
	| Cons(r,rs) -> transpose_helper rs (append_row m_trans r)
;;
let transpose m =
	transpose_helper m Nil
;;
let rec reverse_helper l acc =
	match l with
	| Nil()-> acc
	| Cons(x,xs) -> reverse_helper xs (Cons(x,acc))
;;
let reverse l =
	reverse_helper l Nil
;;
let rec reverse_rows_helper m m_rev =
	match m with
	| Nil()-> m_rev
	| Cons(r,rs) -> reverse_rows_helper rs (reverse Cons(r,m_rev))
;;
let reverse_rows m =
	reverse (reverse_rows_helper m Nil)
;;
let rec append_start m row =
	match row with
	| Nil()-> reverse_rows m
	| Cons(x,xs) ->
		(match m with
		| Nil()-> Cons(Cons(x,Nil),append_start Nil xs)
		| Cons(r,rs) -> Cons(Cons(x,r),append_start rs xs))
;;
let rec new_transpose_helper m m_trans =
	match m with
	| Nil()-> m_trans
	| Cons(r,rs) -> new_transpose_helper rs (append_start m_trans r)
;;
let new_transpose m = reverse_rows (new_transpose_helper m Nil)
;;
let rec prod l1 l2 =
	match l1 with
	| Nil()-> 0
	| Cons(x1,x1s) ->
		(match l2 with
		| Nil()-> 0
		| Cons(x2,x2s) -> (plus (mult x1 x2) (prod x1s x2s)))
;;
let rec prod_mat row mat =
	match mat with
	| Nil()-> Nil
	| Cons(r,rs) -> Cons(prod row r,prod_mat row rs)
;;
let rec mat_mult m1 m2 =
	let m2t = transpose m2 in
	match m1 with
	| Nil()-> Nil
	| Cons(r,rs) -> Cons(prod_mat r m2t,mat_mult rs m2)
;;
let rec lineMult n l1 l2 =
	match l1 with
	| Nil()-> Nil
	| Cons(x,xs) ->
		(match l2 with
		| Nil()-> Cons(mult x n,lineMult n xs Nil)
		| Cons(y,ys) -> Cons(plus (mult x n) y,lineMult n xs ys))
;;
let rec computeLine line m acc =
	match line with
	| Nil()-> acc
	| Cons(x,xs) ->
		match m with
		| Nil()-> Nil
		| Cons(l,ls) -> computeLine xs ls (lineMult x l acc)
;;
let rec mat_mult_jan m1 m2 =
	match m1 with
	| Nil()-> Nil
	| Cons(l,ls) -> Cons(computeLine l m2 Nil, mat_mult_jan ls m2)
;;
let check_mult_sanity m1 m2 =
	land (land (check_matrix m1) (check_matrix m2))
	       (match m1 with
          | Triple(r1, c1, v1) -> (match m2 with
                                   | Triple(r2, c2, v2) -> eqNat c1 c2))
;;
let mult m1 m2 =
	ite (check_mult_sanity m1 m2)
  (match m1 with
     | Triple (m,n,v1) ->
        (match m2 with
         | Triple(m',n',v2) ->
		        let v = mat_mult_jan v1 v2 in
            construct_matrix m n v))
	(error Incompatible_Dimensions)
;;
let rec evaluate expr =
	match expr with
	| Constant(m, n, v) -> construct_matrix m n v
	| Plus(e1, e2) -> plus (evaluate e1) (evaluate e2)
	| Minus(e1, e2) -> minus (evaluate e1) (evaluate e2)
	| Mult(e1, e2) -> mult (evaluate e1) (evaluate e2)
;;
let rec del l i =
	match l with
	| Nil()-> error Out_of_Bounds
	| Cons(x,xs) -> ite (eqNat i 0) xs (Cons(x,del xs (minus i S(0))))
;;
let rec submat m i j =
	match m with
	| Nil()-> Nil
	| Cons(r,rs) -> ite (eqNat i 0) (submat rs (minus i S(0)) j)
		                Cons(del r j,submat rs (minus i S(0)) j)
;;
let remFirstRow m =
	match m with
	| Nil()-> error Out_of_Bounds
	| Cons(r,rs) -> rs

;;

let main x1 x2 = mult x1 x2
 ;;
