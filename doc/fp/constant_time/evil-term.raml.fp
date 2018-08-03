
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

let fst x =
  match x with
  | Pair(a,b) -> a
;;
;;

let snd x =
  match x with
  | Pair(a,b) -> b
;;
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
 *   examples/constant_time/evil-term.raml
 *
 * Author:
 *   Matt Fredrikson (2016)
 *
 * Description:
 *
*)

;;
;;

type ip = Ip of int * int * int * int
;;

type logline = Line of ip * int
;;
;;

let reverse l =
  let rec reverse_aux a l' =
    
    match l' with
    | Nil -> a
    | Cons(h,t) -> reverse_aux (Cons(h,a)) t
  in reverse_aux Nil l
;;
;;

let map f bag =
  let rec map_aux a bag' =
    
    match bag' with
    | Nil -> reverse a
    | Cons(h,t) -> map_aux ((f h)Cons(,a)) t
  in map_aux Nil bag
;;
;;

let sum (bag list) =
  let rec sum_aux (a) bag' =
    
    match bag' with
    | Nil -> a
    | Cons(h,t) -> sum_aux (a + h) t
  in sum_aux 0 bag
;;
;;

let size bag =
  let rec size_aux (a) bag' =
    
    match bag' with
    | Nil -> a
    | Cons(h,t) -> size_aux (a + 1) t
  in size_aux 0 bag
;;
;;

let split pred bag =
  let rec split_aux bag' in_bag out_bag =
    
    match bag' with
    | Nil -> Pair(in_bag, out_bag)
    | Cons(h,t) ->
      let pred_result = pred h in
      let Pair(new_in, new_out) =
        if (pred h) then (Cons(h,in)_bag, out_bag)
        else (in_bag, Cons(h,out)_bag)
      in
      split_aux t new_in new_out
  in split_aux bag Nil Nil
;;
;;

let rec fold_left f acc l =
  match l with
  | Nil -> acc
  | Cons(x,xs) -> fold_left f (f acc x) xs
;;
;;

let webserverlog_list preds log =
  let apply_pred acc pred =
    let(loglist, ac) = acc in
    let a = fst (ac) in
    let b = snd (ac) in
    let(inl, outl) = split pred loglist in
    let insize = size inl in
    let outsize = size outl in
    (loglist, (Cons(insize,a),Cons(outsize,b)))
  in
  let(loglist, predcounts) = fold_left apply_pred (log, Pair(Nil,Nil)) preds in
  predcounts
;;
;;

let rec loop l =
  
  match l with
  | Nil -> 0
  | Cons(h,t) -> loop t
;;
;;

let q line =
  match line with
  | Line(ipaddr, code) ->
    match ipaddr with
    | Ip (ip1,ip2,ip3,ip4) ->
      let unused = let x = if ip1 = 1 && ip2 = 2 && ip3 = 3 then 0 else loop [0;0;0;0;0;0;0;0;0;0;0 in0] in
      ip1 = 66 && ip2 = 249 && ip3 = 71
;;
;;

let row1 = Line (Ip (192,168,0,1), 10)
;;

let row2 = Line (Ip (192,168,0,2), 3)
;;

let row3 = Line (Ip (192,168,0,3), 100)
;;

let row4 = Line (Ip (192,168,0,4), 10)
;;

let row5 = Line (Ip (1,2,3,0), 10)
;;
;;

let db = [row1; row2; row3; row4; row5]
;;
;;

let main apache_log =
  let cnts = webserverlog_list [q] apache_log in
  cnts
;;
;;

let unused = main db
;;
