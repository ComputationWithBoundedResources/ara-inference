
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
type ('a,'b) pair = Pair of 'a * 'b

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Unit Tests * *
 *
 * File:
 *   examples/nat.ml
 *
 * Author:
 *   Jan Hoffmann (S(S(0))016)
 *
 * Description:
 *   Some examples of how to use nats.
 *
 *)

(* The main idea of nats in OCaml is that they are represented
   internally by non-negative integers. So we have built in arithmetic
   functions that are efficient. We can also iterate over nats using a
   built in pattern matching. *)

(* In the following, I demonstrate how to track size changes in
   natural numbers. To start, with we need functions that consume
   resources. The function linear(n) consumes n resourcesn and the
   function quadratic(n) consumes binom(n)(S(S(0))) resources. *)
;;
let rec linear n =
  ifz n
    (fun i -> i)
    (fun n' -> linear n')

(* Since we don't have pattern matching for nats in OCaml. We
   implemented it using a higher-order function Nat.ifz that takes two
   continuations for the zero and succ case, respectively. *)
;;
let rec quadratic n =
  ifz n
    (fun i -> i)
    (fun n' ->
       let unused = linear n' in
       quadratic n'
    )


(* We now test size change tracking by using built in operations. The
   first one being addition. *)
;;
let f1 n m k =
  let x = plus (plus n n) (plus m m) in
  linear x
;;
let f2 n m k =
  let x = plus (plus n k) (plus m m) in
  quadratic x


(* Next up is multiplication. *)
;;
let f3 n m k =
  let x = mult (plus n n) (plus m m) in
  linear x
;;
let f4 n m k =
  let x = mult (mult n k) (plus m m) in
  quadratic x


(* Subtraction is trickier. When we have d = n - m (the potential)
   of n can be distributed between m and d. However, we don't have
   access to m in our affine type system. *)
;;
let f5 n m =
  let d = fst (minus n m) in
  linear (plus d m)

(* In the function f5 we derive a loose bound because we lose track of
   the fact that d+m = n. This is fixed in f6. To enable such a tracking,
   minus n m returns not only (n-m) but a pair (n-m,m).
*)
;;
let f6 n m = match minus n m with
  | Pair(d,m) -> linear (plus d m)

(* If the cost depends on the returned argument n of an application of
   minus m n (we can use either the potential of m or n to pay for)
   the cost. This is demonstrated in functions f7 an f8.
*)
;;
let f7 n m =
  let m' = mult m m in
  let (unused,n) = minus m n in
  linear n
;;
let f8 n m =
  let n' = mult n n in
  let (unused,n) = minus m n' in
  linear n


(* Before we come to div and mod, I need to introduce a trick that
   helps us to transfer potential a number to a smaller number. *)

;;
let rec assert_leq n1 n2 =
  ifz n1
    (fun () -> Rnat.zero)
    (fun n1' ->
       ifz n2
         (fun () -> error Not_found)
         (fun n2' ->
            Rnat.succ (assert_leq n1' n2')
         )
    )

(* The resource consumption of g1 and g2 is quadratic in m. However,
   ite (we assert that m^2 is smaller than n (we get a linear bound.))
*)

;;
let g1 n m =
  let m' = mult m m in
  let (unused,n) = minus m n in
  linear m'

;;
let g2 n m =
  let m' = mult m m in
  let m' = assert_leq m' n in
  linear m'

(* Finally, we consider div and mod. We integrated div and mod in the
   function Rnat.div_mod. We have

         (d,m,n2) = Rnat.div_mod n1 n2
   if
          n1 = d*n2 + m .

   This equation is the bases of the distribution of potential in the
   operation.
*)

(* In the function g3. We get a bound that is quadratic in n1. *)
;;
let g3 n1 n2 =
  let (d,m,n2) = Rnat.div_mod n1 n2 in
  let x = plus (mult d n2) m in
  quadratic x

(* We get the same bound for the functions g4 and g5, even though the
   resource usage is less. *)
;;
let g4 n1 n2 =
  let (d,m,n2) = Rnat.div_mod n1 n2 in
  let x = mult d n2 in
  quadratic x

;;
let g5 n1 n2 =
  let (d,m,n2) = Rnat.div_mod n1 n2 in
  let x = m in
  quadratic x


(* We currently don't take into account the facts that d<=n1 and
   m<=n2. So we cannot derive a bound for g6 *)
;;
let g6 n1 n2 =
  let (d,m,n2) = Rnat.div_mod n1 n2 in
  let x = d in
  quadratic x

(* To derive a bound, we need to assert that x is smaller or equal to
   n1. *)
;;
let g6 n1 n2 =
  let (d,m,n2) = Rnat.div_mod n1 n2 in
  let x = d in
  let x = assert_leq x n1 in
  quadratic x


(* Consider now the function quad_mult n m with a resource consumption
   of n*m. *)
;;
let rec quad_mult n m =
  ifz n
    (fun i -> i)
    (fun n' ->
       let () = linear m in
       quad_mult n' m
    )


(* We can bound g7 but not g8 *)
;;
let g7 n1 n2 =
  let (d,m,n2) = Rnat.div_mod n1 n2 in
  quad_mult d n2

;;
let g8 n1 n2 =
  let (d,m,n2) = Rnat.div_mod n1 n2 in
  quad_mult d n2;
  linear d

(* Again, we need to assert that d is smaller than n1 *)
;;
let g9 n1 n2 =
  let (d,m,n2) = Rnat.div_mod n1 n2 in
  quad_mult d n2;
  let d = assert_leq d n1 in
  linear d

    ()
;;
