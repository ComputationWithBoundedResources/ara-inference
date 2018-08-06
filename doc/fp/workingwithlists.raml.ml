
let rec leqnat x y =
  match y with
  | 0 -> true
  | s(y') -> (match x with
            | s(x') -> leqnat x' y'
            | 0 -> false)
;;
let rec eqnat x y =
  match y with
  | 0 -> (match x with
      | 0 -> true
      | s(x') -> false)
  | s(y') -> (match x with
            | s(x') -> eqnat x' y'
            | 0 -> false)
;;
let rec geqnat x y =
  match x with
   | 0 -> false
   | s(x') -> (match y with
              | 0 -> true
              | s(y') -> geqnat x' y')
;;
let rec ltnat x y =
  match y with
   | 0 -> false
   | s(y') -> (match x with
        | 0 -> true
        | s(x') -> ltnat x' y')
;;
let rec gtnat x y =
  match x with
   | 0 -> false
   | s(x') -> (match y with
             | 0 -> true
             | s(y') -> gtnat x' y')


;;
let ifz n th el = match n with
   | 0 -> th 0
   | s(x) -> el x
;;
let ite b th el = match b with
   | true()-> th
   | false()-> el
;;
let minus n m =
  let rec minus' m n = match m with
        | 0 -> 0
        | s(x) -> (match n with
          | 0 -> m
          | s(y) -> minus' x y)
  in pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | s(x) -> s(plus n x)
;;
type ('a,'b,'c) triple = triple of 'a * 'b * 'c
;;
let rec div_mod n m = match (minus n m) with
  | pair(res,m) -> (match res with
                   | 0 -> triple (0,n,m)
                   | s(x) -> (match (div_mod res m) with
                             | triple(a,rest,unusedm) -> triple(plus s(0) a,rest,m)))
;;
let rec mult n m = match n with
   | 0 -> 0
   | s(x) -> s(plus (mult x m) m)
;;
type bool = true | false
;;
type 'a list = nil | cons of 'a * 'a list
;;
type unit = unit
;;
type ('a,'b) pair = pair of 'a * 'b

(* * * * * * * * * * *
 * resource aware ml *
 * * * * * * * * * * *
 *
 * * *  use cases  * *
 *
 * file:
 *   examples/workingwithlists.raml
 *
 * author:
 *   ankush das (s(s(0))015)
 *
 * description:
 *   the first section ("working with lists") from the ocaml tutorial
 *   "99 problems (solved) in ocaml":
 *     https://ocaml.org/learn/tutorials/99problems.html
 *
 *)


;;
let rec last l =
	match l with
  | nil()-> none
  | cons(x,xs) ->
		match xs with
		| nil()-> some x
		| cons(unused,unused') -> last xs
;;
let rec last_two l =
	match l with
  | nil()-> none
	| cons(x,xs) ->
		match xs with
		| nil()-> none
		| cons(y,ys) ->
			match ys with
			| nil()-> some (x, y)
			| cons(unused,unused') -> last_two xs
;;
let rec at k l =
	match l with
	| nil()-> none
	| cons(h,t) -> ite (eqnat k 0) (some h) (at (minus k s(0)) t)
;;
let rec natat k l =
	match l with
	| nil()-> none
	| cons(h,t) -> ifz k h (fun x -> natat x t)
;;
let length list =
  let rec aux n xyz = match xyz with
    | nil()-> n
    | cons(unused,t) -> aux s(n) t
  in aux 0 list
;;
let rev list =
  let rec aux acc xyz = match xyz with
    | nil()-> acc
    | cons(h,t) -> aux (cons(h,acc)) t
  in aux nil list
;;
let rec eq l1 l2 =
	match l1 with
	| nil()->
		(match l2 with
		| nil()-> true
		| cons(unused,unused') -> false)
	| cons(x1,x1s) ->
		(match l2 with
		| nil()-> false
		| cons(x2,x2s) -> land (eqnat x1 x2) (eq x1s x2s))
;;
let is_palindrome list =
  eq list (rev list)


;;
let rec compress l =
	match l with
	| nil()-> nil
	| cons(x,xs) ->
		(match xs with
		| nil()-> (cons(x,nil))
		| cons(y,ys) -> ite (eqnat x y) (compress xs) (cons(x,compress xs)))
;;
let pack list =

  let rec aux current acc l =
		match l with
    | nil()-> nil
    | cons(a,t) ->
			(match t with
			| nil()-> ((cons(a,cons(current),acc)))
			| cons(b,unused) ->
				ite (eqnat a b) (aux (cons(a,current)) acc t)
				(aux nil (((cons(a,cons(current),acc)) t))))
  in

  rev (aux nil nil list)

;;
let encode list =

  let rec aux count acc l =
		match l with
    | nil()-> nil
		| cons(a,t) ->
			(match t with
    	| nil() -> cons(pair(s(count),a),acc)
    	| cons(b,unused) -> ite (eqnat a b) (aux s(count) acc t)
				(aux 0 (cons(pair(s(count),a),acc)) t))
  in

  rev (aux 0 nil list)

(*
type 'a rle =
  | one(of) 'a
  | many(of) (int * 'a)

;;
let decode list =

  let rec many acc n x =
    ite (n = 0 (acc (many (cons(x,acc)) (ns(0)) x)))
  in

  let rec aux acc l =
		match l with
    | nil()-> acc
		| cons(a,t) ->
			match a with
    	| one(x) -> aux (cons(x,acc)) t
    	| many(n,x) -> aux (many acc n x) t
  in

  aux nil (rev list)

*)
;;
let rec duplicate xyz = match xyz with
  | nil()-> nil
  | cons(h,t) -> cons(h,cons(h,duplicate t))
;;
let replicate list n =

  let rec prepend n acc x =
    ite (eqnat n 0) acc (prepend (minus n s(0)) cons(x,acc) x)
  in

  let rec aux acc xyz = match xyz with
    | nil()-> acc
    | cons(h,t) -> aux (prepend n acc h) t
  in

  (* this could also be written as: list.fold_left (prepend n) nil (list.rev list) *)
  aux nil (rev list)


;;
let drop list n =

  let rec aux i xyz = match xyz with
    | nil()-> nil
    | cons(h,t) -> ite (eqnat i  n) (aux s(0) t) (cons(h,aux (s(i)) t))
  in
  aux S(0) list

;;
(* let split list n =
 *
 *   let rec aux i acc l =
 * 		match l with
 *     | Nil()-> rev acc, Nil
 *     | Cons(h,t) -> ( ite (i = 0 (rev acc, l (aux (iS(0)) (Cons(h,acc)) t))))
 *   in
 *
 *   aux n Nil list
 *
 * ;;
 * let slice list b e =
 *
 *   let rec take n xyz = match xyz with
 *     | Nil()-> Nil
 *     | Cons(h,t) -> ( ite (n = 0 (Nil (Cons(h,take) (nS(0)) t))))
 *   in
 *
 *   let rec drop n l =
 * 		match l with
 *     | Nil()-> Nil
 *     | Cons(h,t) -> ( ite (n = 0 (l (drop (nS(0)) t))))
 *   in
 *
 *   take (e - b + S(0)) (drop (b - S(0)) list)
 *
 * ;;
 * let rec concat l1 l2 =
 * 	match l1 with
 * 	| Nil()-> l2
 * 	| Cons(x,xs) -> ( Cons(x,concat) xs l2)
 * ;;
 * let rotate list n =
 *
 *   let len = length list in
 *   let n = ite (len = 0 (0 ((n mod len + len) mod len in (\* Compute a rotation value between 0 and lenS(0) *\))))
 *   ite (n = 0 (list else))
 *     let a, b = split list n in (concat b a)
 *
 *
 * ;;
 * let rec removeAt n xyz = match xyz with
 *   | Nil()-> Nil
 *   | Cons(h,t) -> ( ite (n = S(0) (t (Cons(h,removeAt) (nS(0)) t))))
 *
 * ;;
 * let rec insertAt x n xyz = match xyz with
 *   | Nil()-> Nil
 *   | (Cons(h,t) -> ( ite (n = S(0) (Cons(x,Cons(h,t) (Cons(h,insertAt) x (nS(0)) t))))))
 *
 * ;;
 * let (--) a b =
 *   let rec aux a b =
 *     ite (gtNat a b (Nil (Cons(a,aux) (a+1) b)))
 *   in
 *
 *   ite (gtNat a b (rev (aux b a) (aux a b)))
 *
 * ;;
 * let random n = S(0)337 mod n
 * ;;
 * let min m n =
 * 	(
 * 	ite (ltNat ((m) (n)) (m))
 * 	(n))
 * ;;
 * let rec rand_select list n =
 *
 *   let rec extract acc n xyz = match xyz with
 *     | Nil()-> error Not_found
 *     | Cons(h,t) -> ( ite (n = 0 (h, (concat acc t) (extract (Cons(h,acc)) (nS(0)) t))))
 *   in
 *
 *   let extract_rand list len =
 *     extract Nil (random len) list
 *   in
 *
 *   let rec aux n acc list len =
 *     ite (n = 0 (acc else))
 *       let picked, rest = extract_rand list len in
 *       ( aux (nS(0)) (Cons(picked,acc)) rest (lenS(0)))
 *   in
 *
 *   let len = length list in
 *
 *   aux (min n len) Nil list len
 *
 * ;;
 * let lotto_select n m = rand_select (S(0) -- m) n
 * (\*
 * let extract k list =
 *
 *   let rec aux k acc emit xyz = match xyz with
 *     | Nil()-> acc
 *     | Cons(h,t) ->
 *       ite (k = S(0) (aux k (emit (Cons(h,Nil)) acc) emit t else))
 * 	let new_emit x = emit (Cons(h,x)) in
 * 	aux k (aux (kS(0)) acc new_emit t) emit t
 *   in
 *
 *   let emit x acc = Cons(x,acc) in
 *
 *   aux k Nil emit list
 *
 * *\)
 * ;;
 * let snd x =
 * 	(
 * 	let (a, b) = x in
 * 	b)
 * ;;
 * let fst x =
 * 	(
 * 	let (a, b) = x in
 * 	a)
 * ;;
 * let rec map f l =
 * 	match l with
 * 	| Nil()-> Nil
 * 	| (Cons(x,Cons(xs) -> ( (f x,map) f xs)))
 * (\*
 * let group list sizes =
 *
 *   let initial = map (fun size -> size, Nil) sizes in
 *
 *   (\* The core of the function. Prepend accepts a list of groups, each with
 *      the number of items that should be added, and prepends the item to every
 *      group that can support it, thus turning (Cons(S(0),a ,Cons( S(S(0)),b ,Cons( 0,c,Nil)))) into
 *      (Cons( (Cons(0,Cons(x,a) ,Cons( S(S(0)),b ,Cons( 0,c ,Nil)))) ,Cons( (Cons(S(0),a ,Cons( S(0),Cons(x,b) ,Cons( 0,c,Nil)))) ,Cons( (Cons( S(0),a ,Cons( S(S(0)),b ,Cons( 0,c ,Nil)))),Nil))))
 *      Again, in the prolog language (for which these questions are intended),
 *      this function is a whole lot simpler.
 *   *\)
 *   let prepend p list =
 *     let emit l acc = Cons(l,acc) in
 *     let rec aux emit acc xyz = match xyz with
 *       | Nil()-> emit Nil acc
 *       | (n,l) as Cons(h,t) ->
 * 	let acc = ite (gtNat n 0 (emit ((nS(0), (Cons(p,Cons(l),t)) acc (acc in)))))
 * 	aux (fun l acc -> emit (Cons(h,l)) acc) acc t
 *     in
 *     aux emit Nil list
 *   in
 *
 *   let rec aux xyz = match xyz with
 *     | Nil()-> (Cons( initial ,Nil))
 *     | Cons(h,t) -> concat (map (prepend h) (aux t))
 *   in
 *
 *   let all = aux list in
 *
 *   (\* Don't forget to eliminate all group sets that have non-full groups *\)
 *   let complete = List.filter (List.for_all (fun (x,_) -> x = 0)) all in
 *
 *   map (map snd) complete
 *
 * *\)
 * ;;
 * let rec insert cmp e xyz = match xyz with
 *   | Nil()-> (Cons(e,Nil))
 *   | (Cons(h,t) -> ( ite (cmp e leqNat h 0 (Cons(e,Cons(h,t) (Cons(h,insert) cmp e t))))))
 *
 * ;;
 * let rec sort cmp xyz = match xyz with
 *   | Nil()-> Nil
 *   | Cons(h,t) -> ( insert cmp h (sort cmp t))
 *
 *
 * (\* Sorting according to length : prepend length, sort, remove length *\)
 * ;;
 * let compare x y =
 * 	(
 * 	ite ((x = y) (0))
 * 	(ite (ltNat ((x) (y)) (S(0))))
 * 	(S(0)))
 * ;;
 * let length_sort lists =
 *   let lists = map (fun list -> length list, list) lists in
 *   let lists = sort (fun a b -> compare (fst a) (fst b)) lists in
 *   map snd lists
 *
 * ()
 * ;; *)
