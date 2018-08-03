
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
 * * *  Use Cases * *
 *
 * File:
 *   examples/power_radio.raml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 * 
 * Description:
 *   Modelling a reactive system whose power consumption depends crucially on
 *   the power consumption of the (GSM) radio. We use the tick metric to model
 *   the power consumption in the function send_msg.
 *)


(* Using the (gsm) radio: Sending a list of integers costs 200 Millijoule to *)
(* power the radio on and 32 Millijoule to per integer that is sent. *)
;;

let send_msg (msg : int list) =

  let transmit x =
    tick(32.)
  in

  let rec send msg =
    match msg with
      | [] -> ()
      | x::xs ->
	transmit x;
	send xs
  in

  let unused = tick(200.) in
  send msg


(* Events that can be handled by the system. *)
;;

type event =
  | Eidle
  | Edata of int list   (*The sensor reads a list of data.*)
  | Esend_buf
  | Esend_all of (int list -> unit)


(* A simple implementation that just sends the sensor data as soon as *)
(* it is produced. *)
;;

let main1 events =

  let rec process events =
    match events with
      | e::es -> 
	begin
	  match e with
	    | Eidle -> ()
	    | Esend_buf -> ()
	    | Esend_all _ -> ()
	    | Edata data -> send_msg data
	end
	; process es
      | [] -> ()
  in

  process events


;;

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | x::xs -> x::(append xs l2)


(* Store sensor data in a buffer and send the buffer only at *)
(* Esend_buf events. *)
;;

let main2 events =

  let rec process events buffer =
    match events with
      | e::es -> 
	let buffer =
	  match e with
	    | Eidle -> buffer
	    | Esend_buf -> send_msg buffer; []
	    | Esend_all _ -> buffer
	    | Edata data -> append data buffer
	in
	process es buffer
      | [] -> ()
  in

  process events []


(* Same functionality of main2. Also add a debugging mode in *)
(* which *all* accumulated data since start is (re)sent. *)
;;

let main3 events =

  let rec process events buffer all_data =
    match events with
      | e::es -> 
	let buffer,all_data =
	  match e with
	    | Eidle -> buffer, all_data
	    | Esend_buf -> send_msg buffer; ([], all_data)
	    | Esend_all _ -> send_msg all_data; (buffer, all_data)
	    | Edata data -> (append data buffer, append data all_data)
	in
	process es buffer all_data
      | [] -> ()
  in

  process events [] []


(* Same functionality as main3. In the debugging mode, now apply *)
(* the function that is provided by the event. *)
;;

let main4 events =

  let rec process events buffer all_data =
    match events with
      | e::es -> 
	let buffer,all_data =
	  match e with
	    | Eidle -> buffer, all_data
	    | Esend_buf -> send_msg buffer; ([], all_data)
	    | Esend_all f -> f all_data; (buffer, all_data)
	    | Edata data -> (append data buffer, append data all_data)
	in
	process es buffer all_data
      | [] -> ()
  in

  process events [] []


(* Similar to main4. However, we ignore Esend_buf events and send the *)
(* buffer after 5 Edata events. *)
;;

let main5 events =

  (* This is an assertion that fails if n<5. *)
  (* It is necessary to inform the analysis that n>=5. *)
  (* There will be a built-in function for this. *)
  let assert_geq_5 n =
    let minus1 n =
      ifz n
	(fun unused -> error(Assert_failure ("", 0, n)))
	(fun n' -> n')
    in
    let n1 = minus1 n in
    let n2 = minus1 n1 in
    let n3 = minus1 n2 in
    let n4 = minus1 n3 in
    let n5 = minus1 n4 in
    ()
  in

  let one = succ 0 in

  let rec process events c buffer =
    match events with
      | e::es -> 
	let buffer,c =
	  match e with
	    | Eidle -> buffer,c
	    | Esend_buf -> buffer,c
	    | Esend_all _ -> buffer,c
	    | Edata data -> 
	      let buffer = append data buffer in
	      if (to_int c) = 5 then begin
		assert_geq_5 c;
		send_msg buffer;
		([], succ 0)
	      end
	      else
		(buffer, succ c)
	in
	process es c buffer
      | [] -> ()
  in
  process events 0 []

;;

;;

let events = 
  let unused = [Eidle; Edata [1;2;3]; Eidle; Edata [4;5;6;7;8]; Edata []; Edata[2;3 in4]
  let unused =  in Esend_all send_msg
  let unused = ; Eidle; Eidle in Esend_buf
  let unused = ; Esend_all (fun data -> send_msg [1;2;3;4]; send_msg data in send_msg data)  
  let unused = ; Edata [1;2;3] in Esend_buf
  let unused = ; Edata [1;2;3] in Esend_buf
  let unused = ; Edata [1;2;3];Edata [130;3];Edata [130;3] in Esend_all send_msg
  ]
in
main1 events,
main2 events,
main3 events,
main4 events,
main5 events






(*
;;

type count =
  | C1
  | C2
  | C3
  | C4
  | C5

;;

let next = function
  | C1 -> C2
  | C2 -> C3
  | C3 -> C4
  | C4 -> C5
  | C5 -> C1



;;

let main5 events =

  let rec process events c buffer =
    match events with
      | e::es -> 
	let buffer,c =
	  match e with
	    | Eidle -> buffer,c
	    | Esend_buf -> buffer,c
	    | Esend_all _ -> buffer,c
	    | Edata data -> 
	      let buffer = append data buffer in
	      let buffer = 
		match c with
		  | C1 -> buffer
		  | C2 -> buffer
		  | C3 -> buffer
		  | C4 -> buffer
		  | C5 -> send_msg buffer; []
	      in
	      (buffer, next c)
	in
	process es c buffer
      | [] -> ()
  in
  process events C1 []

*)
