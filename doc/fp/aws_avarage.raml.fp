
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
 * * *  Use Case  * *
 *
 * File:
 *   examples/aws/sort_average.raml
 *
 * Author:
 *   Jan Hoffmann, Ronghui Gu (2015)
 * 
 * Description:
 *   Using Amazon's DynamoDB to sort students according to their avarage grades. 
 *)

exception Not_found of int * int

;;

type 'a option = None | Some of 'a

;;

let db_query student_id course_id =
  let unused = tick(1.0) in  Some 1.0

;;

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | x::xs -> x::(append xs l2)

;;

let rec partition gt acc l =
  match l with
    | [] -> acc
    | x::xs ->
      match (acc) with
        | Triple(cs,bs,acc) -> 
      match (gt x acc) with
        | Pair(is_greater,acc') -> 
      if is_greater then
  	partition gt (cs,x::bs,acc') xs
      else
  	partition gt (x::cs,bs,acc') xs

;;

let rec quicksort gt acc l = match l with
  | [] -> ([],acc)
  | x::xs ->
    let ys, zs, acc' = partition (gt x) ([],[],acc) xs in
    match (quicksort gt acc' ys) with
        | Pair(l1,acc'') -> 
    match (quicksort gt acc'' zs) with
        | Pair(l2,acc''') -> 
    (append  (x :: l1) l2, acc''')


;;

let rec foldl f acc l =
  match l with
    | [] -> acc
    | x::xs -> foldl f (f acc x) xs

;;

let average_grade student_id course_ids =
  let f acc cid =
    match (acc) with
        | Pair(length,sum) -> 
    let grade = match db_query student_id cid with
      | Some q -> q
      | None -> error(Not_found (student_id,cid))
    in
    (length +. 1.0, sum +. grade)
  in
  match (foldl f (0.0,0.0) course_ids) with
        | Pair(length,sum) -> 
  sum /. length

;;

let greater_eq sid1 sid2 course_ids =
  (average_grade sid1 course_ids >= average_grade sid2 course_ids, course_ids)

;;

let sort_students student_ids course_ids =
  match (quicksort greater_eq course_ids student_ids) with
        | Pair(sorted_sids, acc) -> 
  sorted_sids


;;

let rec map f l =
  match l with
    | [] -> []
    | x::xs -> (f x) :: (map f xs)

;;

let make_table student_ids course_ids =

  let rec mk_table sids cids =
    match sids with
      | [] -> []
      | x::xs ->
	let cid_map =
	  let f cid =
	    let grade =
	      match db_query x cid with
	  	| Some q -> q
	  	| None -> error(Not_found (x,cid))
	    in
	    (cid,grade)
	  in
	  map f cids
	in
	(x,cid_map)::(mk_table xs cids)
  in
  mk_table student_ids course_ids


;;

let rec find f l =
  match l with
    | [] -> error(Not_found (-1,-1))
    | x::xs ->
      match (x) with
        | Pair(key,value) -> 
      if f key then
	value
      else
	find f xs

;;

let lookup sid cid table =
  let cid_map = find (fun (id:int) -> id = sid) table in
  find (fun (id:int) -> id = cid) cid_map


;;

let average_grade' student_id course_ids table =
  let f acc cid =
    match (acc) with
        | Triple(length,sum,table) -> 
    let grade = lookup student_id cid table in
    (length +. 1.0, sum +. grade, table)
  in
  match (foldl f (0.0,0.0,table) course_ids) with
        | Triple(length,sum,table') -> 
  (sum /. length,table')


;;

let greater_eq' course_ids sid1 sid2 table =
  match (average_grade' sid1 course_ids table) with
        | Pair(grade1, table1) -> 
  match (average_grade' sid2 course_ids table1) with
        | Pair(grade2, table2) -> 
  (grade1 >= grade2, table2)


;;

let sort_students_efficient student_ids course_ids =
  match (quicksort (greater_eq' course_ids)  (make_table student_ids course_ids) student_ids) with
        | Pair(sorted_sids, acc) -> 
  sorted_sids

;;

let students = [1;2;3;4;5;6]
;;

let courses = [12;13;14;15;16;17;18]

;;

let _ = sort_students students courses

(* let _ = sort_students_efficient students courses *)


