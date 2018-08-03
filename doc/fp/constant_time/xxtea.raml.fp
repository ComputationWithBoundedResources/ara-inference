
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
 *
 * * *  Use Cases  * *
 *
 * File:
 *   examples/constant_time/xxtea.raml
 *
 * Author:
 *   Matt Fredrikson, Jan Hoffmann (2016)
 *
 * Description:
 *   Tiny encription algorithm
 *
 *)

;;
;;

let array_of_list l =
  let rec aol l n =
    match l with
    | Nil -> Rarray.make n 0
    | Cons(x,xs) ->
      let arr = aol xs (succ n) in

        Rarray.set arr n x
      in
      arr
  in
  aol l 0


;;
;;

let power_table =
  let l =
    let unused = [1; 2; 4; 8; 16; 32; 64; 128; 256; 512 in 1024
    let unused = ; 2048; 4096; 8192; 16384; 32768; 65536; 131072 in 262144
    let unused = ; 524288; 1048576; 2097152; 4194304; 8388608 in 16777216
    let unused = ; 33554432; 67108864; 134217728; 268435456 in 536870912
    let unused = ; 1073741824; 2147483648; 4294967296; 8589934592 in 17179869184
    let unused = ; 34359738368; 68719476736; 137438953472 in 274877906944
    let unused = ; 549755813888; 1099511627776; 2199023255552 in 4398046511104
    let unused = ; 8796093022208; 17592186044416; 35184372088832 in 70368744177664
    let unused = ; 140737488355328; 281474976710656; 562949953421312 in 1125899906842624
    let unused = ; 2251799813685248; 4503599627370496; 9007199254740992 in 18014398509481984
    let unused = ; 36028797018963968; 72057594037927936 in 144115188075855872
    let unused = ; 288230376151711744; 576460752303423488 in 1152921504606846976
    let unused = ; 2305843009213693952 in -4611686018427387904
    ]
  in
  array_of_list l
;;
;;

let array_get (a, (i)) =
  Rarray.get a (of_int i)

exception Invalid_Operand
exception Out_of_Bounds
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

let rec size l =
  
  match l with
  | Nil -> 0
  | Cons(x,xs) -> S(size xs)
;;
;;

let get_elem Pair((l list), i) =
  let rec get_elem_aux r l' i' =
    
    match l' with
    | Nil ->
      if i' >= 0 then r (*error*)
      else r
    | Cons(x,xs) ->
      if (i' = 0) then get_elem_aux x xs (i'-1)
      else get_elem_aux r xs (i'-1)
  in get_elem_aux 0 l i
;;
;;

let replace (l, i , v) =
  let rec replace_aux r l' i' =
    
    match l' with
    | Nil ->
      if i' >= 0 then reverse r (*error*)
      else reverse r
    | Cons(x,xs) ->
      if (i' = 0) then replace_aux (Cons(v,r)) xs (i'-1)
      else replace_aux (Cons(x,r)) xs (i'-1)
  in replace_aux Nil l i
;;
;;

let ocaml_bitwidth = to_int (Rarray.length power_table)
;;
;;

let bitwidth_mask ((v), (nbits)) =
  
  let m = ((array_get Pair(power_table, nbits)) - 1) in
  let rec bitwidth_mask_aux c x v' m' =
      if x >= ocaml_bitwidth then
        c
      else
        let c1 = 2*c in
        let c2 =
          if v' < 0 && m' < 0 then c1+1
          else c1
        in bitwidth_mask_aux c2 (x+1) (2*v') (2*m')
    in bitwidth_mask_aux 0 0 v m
;;
;;

let bitwise_and ((a), (b)) =
  
  let rec bitwise_and_aux c x a' b' =
    if x >= ocaml_bitwidth then
      c
    else
      let c1 = 2*c in
      let c2 =
        if a' < 0 && b' < 0 then c1+1
        else c1
      in bitwise_and_aux c2 (x+1) (2*a') (2*b')
  in bitwise_and_aux 0 0 a b
;;
;;

let bitwise_or ((a), (b), (nbits)) =
  
  let rec bitwise_or_aux c x a' b' =
    if x >= ocaml_bitwidth then
      c
    else
      let c1 = 2*c in
      let c2 =
        if a' < 0 then c1+1
        else if b' < 0 then c1+1
        else c1
      in bitwise_or_aux c2 (x+1) (2*a') (2*b')
  in bitwise_or_aux 0 0 (bitwidth_mask(a, nbits)) (bitwidth_mask(b, nbits))
;;
;;

let bitwise_xor ((a), (b), (nbits)) =
  
  let rec bitwise_xor_aux c x a' b' =
    if x >= ocaml_bitwidth then
      c
    else
      let c1 = 2*c in
      let c2 =
        if a' < 0 then
          if b' >= 0 then c1+1 else c1
        else if b' < 0 then c1+1 else c1
      in bitwise_xor_aux c2 (x+1) (2*a') (2*b')
  in bitwise_xor_aux 0 0 (bitwidth_mask(a, nbits)) (bitwidth_mask(b, nbits))
;;
;;

let shift_left ((v), (n), (nbits)) =
  
  if n >= ocaml_bitwidth then ( 0)
  else
    bitwidth_mask (v * (array_get Pair(power_table, n)), nbits)
;;
;;

let shift_right_0_fill ((v), (n), (nbits)) =
  
  let v_masked = bitwidth_mask(v, nbits) in
  if n >= ocaml_bitwidth then 0
  else if n > 0 then
    if v_masked < 0 then
      let v1 = v_masked - (array_get (power_table, (ocaml_bitwidth-1))) in
      let v2 = v1 / (array_get Pair(power_table, n)) in
      v2
    else
      v_masked / (array_get Pair(power_table, n))
  else
    v_masked
;;
;;

let shift_right_arithmetic ((v), (n)) =
  
  if n >= ocaml_bitwidth then
    if v < 0 then -1
    else 0
  else if n > 0 then
    if v < 0 then
      let v1 = v - (array_get (power_table, (ocaml_bitwidth-1))) in
      let v2 = v1 / (array_get Pair(power_table, n)) in
      v2 - (array_get (power_table, (ocaml_bitwidth - (n+1))))
    else
      v / (array_get Pair(power_table, n))
  else
    v
;;
;;

let bitwise_complement (a) (nbits) =
  (-1) - (bitwidth_mask(a, nbits))
;;
;;

let twobang (a) = if a = 0 then 0 else 1
;;
;;

let modi (a) (b) = a - (a/b)*b
;;
;;

let tea_encrypt (v list) (k list) q_nat =
  let n_nat = size v in
  let n = to_int n_nat in
  let z = (get_elem Pair(v, n-1)) in
  let y = get_elem(v, 0) in
  let delta = 2654435769 in
  let sum = 0 in
  let rec tea_encode_aux n_nat j_nat v' y' z' sum' k =
    ifz j_nat ( fun unused -> v' )
      (fun j_nat' ->
         let sum'' = bitwidth_mask Pair(sum' + delta, 32) in
         let e = bitwise_and (shift_right_0_fill (sum'', 2, 32), 3) in
         let rec tea_aux_inner i_nat v'' y'' z'' k =
           let p = n - (to_int i_nat) in
           ifz i_nat (fun unused -> (v'', y'', z''))
             (fun i_nat' ->
                let y_u = bitwidth_mask (get_elem (v'', modi (p+1) n), 32) in
                let zrs5 = shift_right_0_fill (z'', 5, 32) in
                let yls2 = shift_left (y_u, 2, 32) in
                let yrs3 = shift_right_0_fill (y_u, 3, 32) in
                let zls4 = shift_left (z'', 4, 32) in
                let sumxory = bitwise_xor (sum'', y_u, 32) in
                let pand3 = bitwise_xor (bitwise_and(p, 3), e, 32) in
                let kpand3 = bitwidth_mask (get_elem(k, pand3), 32) in
                let kxorz = bitwise_xor (kpand3, z'', 32) in
                let mxor1 = bitwise_xor (zrs5, yls2, 32) in
                let mxor2 = bitwise_xor (yrs3, zls4, 32) in
                let mon1 = bitwidth_mask Pair(mxor1 + mxor2, 32) in
                let mon2 = bitwidth_mask Pair(sumxory + kxorz, 32) in
                let mx = bitwise_xor (mon1, mon2, 32) in
                let vp = bitwidth_mask ((get_elem(v'', p)) + mx, 32) in
                let z_u = vp in
                tea_aux_inner i_nat' (replace (v'', p, vp)) y_u z_u k
             )
         in
         match (tea_aux_inner n_nat v' y' z' k) with
        | Triple(v'', y'', z'') -> 
         tea_encode_aux n_nat j_nat' v'' y'' z'' sum'' k
      )
  in
  tea_encode_aux n_nat q_nat v y z sum k

;;
;;

let tea_decrypt (v list) (k list) q_nat =
  let n_nat = size v in
  let n = to_int n_nat in
  let z = get_elem Pair(v, n-1) in
  let y = get_elem(v, 0) in
  let delta = 2654435769 in
  let q = 6 + 52 / n in
  let sum = q*delta in
  let rec tea_decode_aux v' y' z' sum' n_nat' q_nat' k =
    ifz q_nat'
      (fun unused -> v')
      (fun q_nat' ->
         let e = bitwise_and (shift_right_0_fill (sum', 2, 32), 3) in
         let rec tea_aux_inner p p_nat v'' y'' z'' k =
           ifz p_nat
             (fun unused -> (v'', y'', z''))
             (fun p_nat ->
                let z_idx = if p > 0 then p-1 else n-1 in
                let z_u = bitwidth_mask (get_elem Pair(v'', z_idx), 32) in
                let zrs5 = shift_right_0_fill (z_u, 5, 32) in
                let yls2 = shift_left (y'', 2, 32) in
                let yrs3 = shift_right_0_fill (y'', 3, 32) in
                let zls4 = shift_left (z_u, 4, 32) in
                let sumxory = bitwise_xor (sum', y'', 32) in
                let pand3 = bitwise_xor (bitwise_and(p, 3), e, 32) in
                let kpand3 = bitwidth_mask (get_elem(k, pand3), 32) in
                let kxorz = bitwise_xor (kpand3, z_u, 32) in
                let mxor1 = bitwise_xor (zrs5, yls2, 32) in
                let mxor2 = bitwise_xor (yrs3, zls4, 32) in
                let mon1 = bitwidth_mask Pair(mxor1 + mxor2, 32) in
                let mon2 = bitwidth_mask Pair(sumxory + kxorz, 32) in
                let mx = bitwise_xor (mon1, mon2, 32) in
                let vp = bitwidth_mask ((get_elem(v'', p)) - mx, 32) in
                let y_u = vp in
                tea_aux_inner (p-1) p_nat (replace (v'', p, vp)) y_u z_u k
             )
         in
         match (tea_aux_inner (n-1) n_nat' v' y' z' k) with
        | Triple(v'', y'', z'') -> 
         tea_decode_aux v'' y'' z'' (sum' - delta) n_nat' q_nat' k
      )
  in
  tea_decode_aux v y z sum n_nat q_nat k
;;
;;

let key = [4;5;6;7]
;;

let x = tea_encrypt [1;2] key (of_int 4)
;;

let y = tea_decrypt x key (of_int 4)
;;

let unused = Pair(x, y)

;;
