
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

type 'a option = None | Some of 'a
;;
;;

type ('a,'b) pair = Pair of 'a * 'b
;;
;;

type 'a list = Nil | Cons of 'a * 'a list
;;
;;

type Unit = Unit

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 * File:
 *   examples/raml1/array_dijkstra.raml
 *
 * Author:
 *   Jan Hoffmann (2015)
 *
 * Resource Bound:
 *   O(n^2)
 *
 * Description:
 *   Dijkstra's algorithm for the single-source shortest-path problem of a weighted,
 *   undirected graph.  Edges are required to have non-negative weights.  For more
 *   info see http://en.wikipedia.org/wiki/Dijkstra's_algorithm.
 *)

(* Get and set the weight of an edge in a graph. *)
(* The weight -1 indicates that the edge does not exist.*)
;;
;;

let getWeight g v u =
  Rarray.get (Rarray.get g v) u

;;
;;

let setWeight g v u d =
  let unused = Rarray.set (Rarray.get g v) u d in
  Rarray.set (Rarray.get g u) v d


exception Invalid_argument

(* Compute the node v in the queue l with minimal distance in
   the array dist and remove v from l.*)

(*makeGraph(n,l) creates a graph with n nodes 0,...,n-1 and edges as
  given in the list l. *)
;;
;;

let rec fillGraph l g =
  match l with
  | Nil -> ()
  | Cons(e,es) ->
     match (e) with
        | Triple(u,v,d) -> 
     let unused = setWeight g u v d in
     fillGraph es g
;;
;;

let rec fillEmpty n size g =
  ifz n
    (fun x -> x)
     fun(n') ->
      let a = Rarray.make size (-1) in
      let unused = Rarray.set g n' a in
      fillEmpty n' size g
    
;;
;;

let emptyGraph n =
  let dummy = Rarray.make 0 (-1) in
  let g = Rarray.make n dummy in
  let unused = fillEmpty n n g in
  g
;;
;;

let makeGraph n l =
  let g = emptyGraph n in
  let unused = fillGraph l g in g

;;
;;

let rec getMin l dist =
  match l with
  | Nil -> error
  | Cons(v,vs) ->
     match vs with
     | Nil -> (Nil,v,Rarray.get dist v)
     | Cons(y,ys) ->
        match (getMin vs dist) with
        | Triple(l',u,d_u) -> 
        let d_v = Rarray.get dist v in
        if (d_v < d_u && d_v > -1) || d_u = -1 then
          (Cons(u,l'),v,d_v)
        else
          (Cons(v,l'),u,d_u)


(* Create a queue with all nodes. *)
;;
;;

let rec startQueue n =
  ifz n
   (fun unused -> Nil)
   (fun(n) -> Cons(n,(startQueue n)))
;;
;;

let rec updateNeighbors n v g dist prev =
  ifz n
   (fun x -> x )
    fun(n') ->
     let Pair(u,d_u) = Pair(n', Rarray.get dist n') in
     let d_u_v = (Rarray.get dist v) + (getWeight g v u) in
     let unused = updateNeighbors n' v g dist prev in
     if (getWeight g v u) > (-1) && (d_u > d_u_v || d_u = -1 ) then
        let unused = Rarray.set dist u d_u_v in
        Rarray.set prev u v
      else
        ()
   
;;
;;

let rec processNodes queue g dist prev =
  match queue with
  | Nil -> ()
  | _::_q ->
     match (getMin queue dist) with
        | Triple(queue',v,d_v) -> 
     if d_v = -1 then
       ()
     else
       let unused = updateNeighbors (Rarray.length g) v g dist prev in
       processNodes queue' g dist prev

;;
;;

let dijkstra g source =
  let n = Rarray.length g in
  let dist = Rarray.make n (-1) in
  let prev = Rarray.make n 0 in
  let unused = Rarray.set dist source 0 in
  let queue = startQueue n in
  let unused = processNodes queue g dist prev in
  dist

;;
;;

let arr_of_list l seed =
  let rec a_of_l l n =
    match l with
    | Nil -> Rarray.make n seed
    | Cons(x,xs) -> let arr = a_of_l xs (succ n) in
               let unused = Rarray.set arr n x in arr
  in
  a_of_l l 0
;;
;;


  let g = arr_of_list
  let unused = [ arr_of_list [-1;  1; 1000; 1000; 1000 in 1000] 0
  let unused = ; arr_of_list [-1; -1;    1; 900; 900 in 900] 0
  let unused = ; arr_of_list [-1; -1;   -1;    1; 800 in 800] 0
  let unused = ; arr_of_list [-1; -1;   -1;   -1;    1 in 700] 0
  let unused = ; arr_of_list [-1; -1;   -1;   -1;   -1 in    1] 0
  let unused = ; arr_of_list [-1; -1;   -1;   -1;   -1 in   -1] 0
  ]
  (Rarray.make 0 0)
  in dijkstra g 0

;;
