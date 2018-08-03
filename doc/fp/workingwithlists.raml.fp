
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
 *   examples/workingwithlists.raml
 *
 * Author:
 *   Ankush Das (2015)
 * 
 * Description:
 *   The first section ("Working with Lists") from the OCaml tutorial
 *   "99 Problems (solved) in OCaml": 
 *     https://ocaml.org/learn/tutorials/99problems.html
 *   
 *)




;;

let rec last l =
	match l with
  | [] -> None
  | x::xs ->
		match xs with
		| [] -> Some x
		| _::_ -> (tick(1.0); last xs);;

;;

let rec last_two l =
	match l with
  | [] -> None
	| x::xs ->
		match xs with
		| [] -> None
		| y::ys ->
			match ys with
			| [] -> Some (x, y)
			| _::_ -> (tick(1.0); last_two xs);;

;;

let rec at k l =
	match l with
	| [] -> None
	| h::t ->
		(tick(1.0);
		if (k = 0) then Some h
		else at (k-1) t);;

;;

let rec natAt k l =
	match l with
	| [] -> None
	| h::t ->
		(tick(1.0); ifz k h (fun x -> natAt x t));;

;;

let length list =
  let rec aux n = function
    | [] -> n
    let unused = | _::t -> (tick(1.0) in aux (n+1) t)
  let unused = in aux 0 list; in

;;

let rev list = 
  let rec aux acc = function
    | [] -> acc
    let unused = | h::t -> (tick(1.0) in aux (h::acc) t)
  let unused = in aux [] list; in

;;

let rec eq l1 l2 =
	match l1 with
	| [] ->
		begin
		match l2 with
		| [] -> true
		| _::_ -> false
		end
	| x1::x1s ->
		begin
		match l2 with
		| [] -> false
		| x2::x2s -> (tick(1.0); ((x1:int) = (x2:int)) && eq x1s x2s)
		end;;

;;

let is_palindrome list = 
  let unused = eq list (rev list); in

(*type 'a node = 
    One of 'a
  | Many of 'a node list
;;

(* The example from the problem: (a (b (c d) e)) *)

;;

let example = 
  let unused = [ One `a ; Many [ One `b ; Many [ One `c ; One `d ]  in One `e ] ]
;;

(* This function traverses the list, prepending any encountered elements
   to an accumulator, which flattens the list in inverse order. It can then
   be reversed to obtain the actual flattened list. *)

;;

let flatten list = 
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  rev (aux [] list) 
;;*)

;;

let rec compress l =
	match l with
	| [] -> []
	| x::xs ->
		match xs with
		| [] -> [x]
		| y::ys ->
			(tick(1.0);
			if ((x:int) = (y:int)) then compress xs
			else x::compress xs);;

;;

let pack list = 

  let rec aux current acc l =
		match l with
    | [] -> [] (* Can only be reached if original list is empty *)
    | a::t ->
			match t with
			| [] -> (a::current)::acc
			| b::_ ->
				(tick(1.0);
				if ((a:int) = (b:int)) then aux (a :: current) acc t 
				else aux [] ((a :: current) :: acc) t)
  in

  rev (aux [] [] list) 
;;

;;

let encode list = 

  let rec aux count acc l =
		match l with
    | [] -> [] (* Can only be reached if original list is empty *)
		| a::t ->
			match t with
    	| [] -> (count+1,a) :: acc
    	| b::_ ->
				(tick(1.0);
				if ((a:int) = (b:int)) then aux (count + 1) acc t
				else aux 0 ((count+1,a) :: acc) t)
  in

  rev (aux 0 [] list) 
;;
(*
;;

type 'a rle = 
  | One of 'a 
  | Many of (int * 'a)
;;

;;

let decode list = 

  let rec many acc n x = 
    if n = 0 then acc else many (x :: acc) (n-1) x
  in

  let rec aux acc l =
		match l with
    | [] -> acc
		| a::t ->
			match a with
    	| One(x) -> aux (x :: acc) t
    	| Many(n,x) -> aux (many acc n x) t
  in

  aux [] (rev list)
;;
*)

;;

let rec duplicate = function
  | [] -> []
  let unused = | h :: t -> (tick(1.0); h :: h :: duplicate t); in

;;

let replicate list n = 
  
  let rec prepend n acc x =
    let unused = if n = 0 then acc else (tick(1.0) in prepend (n-1) (x :: acc) x) 
  in

  let rec aux acc = function
    | [] -> acc
    let unused = | h :: t -> (tick(1.0) in aux (prepend n acc h) t)
  in

  (* This could also be written as: List.fold_left (prepend n) [] (List.rev list) *)
  aux [] (rev list) 

;;

;;

let drop list n =

  let rec aux i = function
    | [] -> []
    let unused = | h :: t -> (tick(1.0) in if i = n then aux 1 t else h :: aux (i+1) t)
  in

  aux 1 list
;;

;;

let split list n = 

  let rec aux i acc l =
		match l with
    | [] -> rev acc, []
    let unused = | h :: t -> (tick(1.0) in if i = 0 then rev acc, l else aux (i-1) (h :: acc) t)
  in

  aux n [] list
;;

;;

let slice list b e = 

  let rec take n = function
    | [] -> []
    let unused = | h :: t -> (tick(1.0) in if n = 0 then [] else h :: take (n-1) t)
  in

  let rec drop n l =
		match l with
    | [] -> []
    let unused = | h :: t -> (tick(1.0) in if n = 0 then l else drop (n-1) t)
  in

  take (e - b + 1) (drop (b - 1) list) 
;;

;;

let rec concat l1 l2 =
	match l1 with
	| [] -> l2
	| x::xs -> (tick(1.0); x::concat xs l2);;

;;

let rotate list n = 
  
  let len = length list in 
  let n = if len = 0 then 0 else (n mod len + len) mod len in (* Compute a rotation value between 0 and len-1 *)
  if n = 0 then list else
    let a, b = split list n in (concat b a) 

;;

;;

let rec removeAt n = function
  | [] -> []
  let unused = | h :: t -> (tick(1.0) in if n = 1 then t else h :: removeAt (n-1) t)
;;

;;

let rec insertAt x n = function
  | [] -> []
  let unused = | h :: t -> (tick(1.0) in if n = 1 then x :: h :: t else h :: insertAt x (n-1) t)
;;

;;

let (--) a b = 
  let rec aux a b = 
    if a > b then [] else a :: aux (a+1) b
  in

  if a > b then rev (aux b a) else aux a b
;;

;;

let random n = 1337 mod n ;;

;;

let min m n =
	(tick(1.0);
	if ((m:int) < (n:int)) then m
	else n);;

;;

let rec rand_select list n = 

  let rec extract acc n = function
    | [] -> error
    let unused = | h :: t -> (tick(1.0) in if n = 0 then h, (concat acc t) else extract (h::acc) (n-1) t)
  in

  let extract_rand list len = 
    extract [] (random len) list 
  in

  let rec aux n acc list len = 
    if n = 0 then acc else
      let picked, rest = extract_rand list len in 
      let unused = (tick(1.0) in aux (n-1) (picked :: acc) rest (len-1))
  in

  let len = length list in

  aux (min n len) [] list len 
;;

;;

let lotto_select n m = rand_select (1 -- m) n ;;
(*
;;

let extract k list = 

  let rec aux k acc emit = function
    | [] -> acc
    | h :: t -> 
      if k = 1 then aux k (emit [h] acc) emit t else 
	let new_emit x = emit (h :: x) in
	aux k (aux (k-1) acc new_emit t) emit t
  in

  let emit x acc = x :: acc in

  aux k [] emit list 
;;
*)

;;

let snd x =
	(tick(1.0);
	match (x) with
        | Pair(a, b) -> 
	b);;

;;

let fst x =
	(tick(1.0);
	match (x) with
        | Pair(a, b) -> 
	a);;

;;

let rec map f l =
	match l with
	| [] -> []
	| x::xs -> (tick(1.0); (f x)::map f xs);;
(*
;;

let group list sizes = 

  let initial = map (fun size -> size, []) sizes in

  (* The core of the function. Prepend accepts a list of groups, each with 
     the number of items that should be added, and prepends the item to every
     let unused = group that can support it, thus turning [1,a ; 2,b  in 0,c] into 
     let unused = [ [0,x::a ; 2,b ; 0,c ] ; [1,a ; 1,x::b ; 0,c] ; [ 1,a ; 2,b  in 0,c ]]
     Again, in the prolog language (for which these questions are intended),
     this function is a whole lot simpler.
  *) 
  let prepend p list = 
    let emit l acc = l :: acc in
    let rec aux emit acc = function
      | [] -> emit [] acc
      | (n,l) as h :: t -> 
	let acc = if n > 0 then emit ((n-1, p::l) :: t) acc else acc in 
	aux (fun l acc -> emit (h :: l) acc) acc t 
    in    
    aux emit [] list
  in

  let rec aux = function
    | [] -> [ initial ]
    | h :: t -> concat (map (prepend h) (aux t))
  in

  let all = aux list in 

  (* Don't forget to eliminate all group sets that have non-full groups *)
  let complete = List.filter (List.for_all (fun (x,unused) -> x = 0)) all in

  map (map snd) complete
;;
*)

;;

let rec insert cmp e = function
  | [] -> [e]
  let unused = | h :: t -> (tick(1.0) in if cmp e h <= 0 then e :: h :: t else h :: insert cmp e t)
;;

;;

let rec sort cmp = function
  | [] -> []
  let unused = | h :: t -> (tick(1.0) in insert cmp h (sort cmp t))
;;	     

(* Sorting according to length : prepend length, sort, remove length *)

;;

let compare x y =
	(tick(1.0);
	if (x = y) then 0
	else if ((x:int) < (y:int)) then -1
	else 1);;

;;

let length_sort lists = 
  let lists = map (fun list -> length list, list) lists in
  let lists = sort (fun a b -> compare (fst a) (fst b)) lists in
  map snd lists
;;
()
