
let fst x =
  match x with
  | Pair(a,b) -> a
;;
let snd x =
  match x with
  | Pair(a,b) -> b
;;

let rec leqNat y x =
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
  match y with
  | 0 -> True
  | S(y') -> (match x with
             | 0 -> False
             | S(x') -> geqNat x' y')
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
             | S(y') -> gt x' y')
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
let minus n m =
  let rec minus' m n = match m with
        | 0 -> 0
        | S(x) -> (match n with
          | 0 -> m
          | S(y) -> minus' x y)
  in Pair(minus' n m,m)
;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> (match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> (match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)))

;;
type bool = True | False
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type nat = 0 | S of nat
;;
type Unit = Unit

;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
;;
let ite b th el = match b with
   | True()-> th
   | False()-> el
;;
let ite2 b th el = match b with
   | True()-> th
   | False()-> el
;;
let ite3 b th el = match b with
   | True()-> th
   | False()-> el
;;

type ('a,'b,'c) triple = Triple of 'a * 'b * 'c

;;
type ('a,'b) pair = Pair of 'a * 'b


(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Case  * *
 *
 * File:
 *   examples/aws/sort_average.raml
 *
 * Author:
 *   Jan Hoffmann, Ronghui Gu (S(S(0))015)
 *
 * Description:
 *   Using Amazon's DynamoDB to sort students according to their avarage grades.
 *)
;;
type ('a,'b) exception = Not_found of 'a * 'b
;;
type 'a option = None | Some of 'a
;;
let db_query student_id course_id =
    Some(S(0))
;;
let rec append l1 l2 =
  match l1 with
    | Nil()-> l2
    | Cons(x,xs) -> Cons(x,(append xs l2))

;;
let rec partition gt acc l =
  match l with
    | Nil()-> acc
    | Cons(x,xs) ->
      (match acc with
       | Triple(cs,bs,accN) ->
          (match gt x accN with
           | Pair(is_greater,courseIds) -> ite is_greater
  	                                         (partition gt Triple(cs,Cons(x,bs),courseIds) xs)
  	                                         (partition gt Triple(Cons(x,cs),bs,courseIds) xs)))
 ;;
let rec quicksort gt acc l = match l with
  | Nil()-> Pair(Nil,acc)
  | Cons(x,xs) ->
    match (partition (gt x) Triple(Nil,Nil,acc) xs) with
    | Triple(ys, zs, acc') ->
    (match (quicksort gt acc' ys) with
     | Pair(l1,acc'') ->
        (match (quicksort gt acc'' zs) with
         | Pair(l2,acc''') -> Pair(append  (Cons(x,l1)) l2, acc''')))

;;
let rec foldl f acc l =
  match l with
    | Nil()-> acc
    | Cons(x,xs) -> foldl f (f acc x) xs
;;
let average_grade student_id course_ids =
  let f acc cid =
    match (acc) with
        | Pair(length,sum) ->
    let grade = match db_query student_id cid with
      | Some(q) -> q
      | None()-> error (Not_found (student_id,cid))
    in Pair(S(length), plus sum grade)
  in match (foldl f Pair(0,0) course_ids) with
        | Pair(length,sum) -> match (div_mod sum length) with
           | Triple(dv,md,unused) -> dv
;;

let greater_eq sid1 sid2 course_ids =
  Pair(geq (average_grade sid1 course_ids) (average_grade sid2 course_ids), course_ids)
;;
let sort_students student_ids course_ids =
  match (quicksort greater_eq course_ids student_ids) with
        | Pair(sorted_sids, acc) -> sorted_sids

;;
let rec map f l =
  match l with
    | Nil()-> Nil
    | Cons(x,xs) -> Cons(f x,(map f xs))
;;
let rec find f l =
  match l with
    | Nil()-> error
    | Cons(x,xs) ->
      (match x with
       | Pair(key,value) -> ite2 (f key) value (find f xs))
;;
let rec find2 f l =
  match l with
    | Nil()-> error
    | Cons(x,xs) ->
      (match x with
       | Pair(key,value) -> ite3 (f key) value (find2 f xs))
;;

let lookup sid cid table =
  let cid_map = find (fun sid' -> eqNat sid sid') table
  in find2 (fun sid' -> eqNat sid' cid) cid_map

;;
let rec foldl2 f acc l =
  match l with
    | Nil()-> acc
    | Cons(x,xs) -> foldl2 f (f acc x) xs
;;
let average_grade' student_id course_ids table =
  let f acc cid = match acc with
         | Pair(length,sum) ->
            let grade = lookup student_id cid table
            in Pair(S(length), plus sum grade)
  in match (foldl2 f Pair(0,0) course_ids) with
     | Pair(length,sum) -> (match (div_mod sum length) with
                            | Triple(dv,md,unused) -> dv)

;;
let make_table student_ids course_ids =

  let rec mk_table sids cids =
    match sids with
      | Nil()-> Nil
      | Cons(x,xs) ->
	let cid_map =
	  let f cid =
	    let grade =
	      match db_query x cid with
	  	| Some(q) -> q
	  	| None()-> error (Not_found (x,cid))
	    in
	    Pair(cid,grade)
	  in
	  map f cids
	in
	Cons(Pair(x,cid_map),(mk_table xs cids))
  in
  mk_table student_ids course_ids

;;
let greater_eq' course_ids sid1 sid2 table =
  let grade1 = average_grade' sid1 course_ids table in
  let grade2 = average_grade' sid2 course_ids table in
  Pair(geqNat grade1 grade2, table)

;;
let sort_students_efficient student_ids course_ids =
  match (quicksort (greater_eq' course_ids)  (make_table student_ids course_ids) student_ids) with
        | Pair(sorted_sids, acc) -> sorted_sids
;;


(* let students = (Cons(S(0),Cons(S(S(0)),Cons(S(S(S(0))),Cons(S(S(S(S(0)))),Cons(S(S(S(S(S(0))))),Cons(S(S(S(S(S(S(0)))))),Nil)))))))
 * let courses = [12;13;14;15;16;17;18]
 * ;; *)
(* let main students courses = sort_students students courses *)

let main students courses = sort_students_efficient students courses
;;


