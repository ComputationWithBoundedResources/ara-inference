
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
 *   examples/running.raml
 *
 * Author:
 *   Jan Hoffmann (2015)
 * 
 * Description:
 *   Running example in the 2015 TR.
 *   
 *)



;;

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | x::xs -> x::(append xs l2)


;;

let rec partition f l =
  match l with
    | [] -> ([],[])
    | x::xs ->
      match (partition f xs) with
        | Pair(cs,bs) -> 
      if f x then
	(cs,x::bs)
      else
	(x::cs,bs)


;;

let rec quicksort gt = function
  | [] -> []
  | x::xs ->
      let ys, zs = partition (gt x) xs in
      append (quicksort gt ys) (x :: (quicksort gt zs))

exception Inv_arg

;;

type ('a,'b) ablist =
    Acons of 'a * ('a,'b) ablist
  | Bcons of 'b * ('a,'b) ablist
  | Nil

;;

let rec abmap f g abs =
  match abs with
  | Acons (a,abs') -> Acons(f a, abmap f g abs')
  | Bcons (b,abs') -> Bcons(g b, abmap f g abs')
  | Nil -> Nil

;;

let asort gt abs =
  abmap (quicksort gt) (fun x -> x) abs

;;

let asort' gt abs =
  abmap (quicksort gt) (fun _ -> error) abs

;;

let btick = abmap (fun a -> a) (fun b -> tick 2.5; b)
	     
;;

let rec abfoldr f g acc abs =
  match abs with
  | Acons (a,abs') ->
     let acc' = abfoldr f g acc abs' in
     f a acc'
  | Bcons (b,abs') ->
     let acc' = abfoldr f g acc abs' in
     g b acc'
  | Nil ->
     acc

;;

let cons_all abs =
  let f x y =
    let fa = fun x acc -> error in
    let fb = fun b acc -> Bcons(b+x,acc) in
    abfoldr fa fb Nil y
  in
  let g x y = Bcons (x,y) in  
  abfoldr f g Nil abs

;;

let abs = Acons ([1;2],Bcons (3, Bcons (4, Nil)))

;;

let e1 () = asort  (>) (Acons ([1;2],Bcons (0, Acons ([3;4], Nil))))
;;

let e2 () = asort' (>) (Acons ([1;2],Bcons (0, Acons ([3;4], Nil))))
;;

let e3 () = btick (Acons ([1;2],Bcons (0, Acons ([3;4], Nil))))

		   
	       
;;

let abs = Acons (100, Bcons (2, Bcons (3, Acons(4,Nil))))
;;

cons_all abs  

    
