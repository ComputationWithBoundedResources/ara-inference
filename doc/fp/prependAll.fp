type nat = 0 | S of nat
;;

type 'a list = Nil | Cons of 'a * 'a list
;;

(*
map f [] = [];
map f (x:xs) = (f x) : (map f xs);

append [] ys = ys;
append (x:xs) ys = x : (append xs ys);

prependAll xs ys = map (append xs) ys;
*)

let rec map f xs =
  match xs with
  | Nil -> Nil
  | Cons(x,xs') -> Cons(f x, map f xs')
;;


let rec append xs ys =
  match xs with
  | Nil -> ys
  | Cons(x,xs') -> Cons(x, append xs' ys)
;;


let prependAll xs ys = map (append xs) ys
;;
