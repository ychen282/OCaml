(* Lecture 12 : References *)

let r = ref 0
let s = ref 0
let a = r=s
let a' = r==s
let _ = r := 3
let x = !s + !r
let t = r
let b = s=t
let c = r=t
let _ = t := 5
let y = !s + !r
let z = !t + !r


let test () = 
  let pi   = 3.14 in                              (* 1 *)
  let area = (fun r -> pi *. r *. r) in           (* 2 *)
  let a2   = area (2.0) in                        (* 3 *)
  let pi   = 6.0  in                              (* 4 *)
  let a3 = area 2.0 in
    (print_string ("Area a2 = " ^ string_of_float a2 ^ "\n");
     print_string ("Area a3 = " ^ string_of_float a3 ^ "\n"))
;;



let test_update () = 
  let pi   = ref 3.14 in                              (* 1 *)
  let area = (fun r -> !pi *. r *. r) in              (* 2 *)
  let a2   = area (2.0) in                            (* 3 *)
  let _    = (pi := 6.0) in                           (* 4 *)
  let a3 = area 2.0 in
    (print_string ("Area a2 = " ^ string_of_float a2 ^ "\n");
     print_string ("Area a3 = " ^ string_of_float a3 ^ "\n"))
;;


(* Rotating values - 3 different versions *)
let rot3a (a, b, c) =
    let t = !a in
    (a := !b; b := !c; c := t)
;;

let rot3b (a, b, c) =
  let (x, y, z) = (!a, !b, !c)  in
  (a := y; b := z; c := x)
;;

let rot3c (a, b, c) =
  let ({contents = x}, {contents = y}, {contents = z }) = (a, b, c) in
  (a := y; b := z; c := x)
;;



(* Imperative programming using references *)

let imperative_fact n =
  begin
    let result = ref 1 in
    let i = ref 0 in
    let rec loop () =
      if !i = n then ()
      else (i := !i + 1; result := !result * !i; loop ())
    in
    (loop (); !result)
  end


(* Name generation using a global variable *)
(*  counter to guarantee new variables *)
let counter = ref 0

  (* newName () ===> a,  where a is a new name *)
  (*
    Names are described by strings denoting natural numbers.
  *)
let newName () =
  (counter := !counter + 1;
   "a" ^ string_of_int (!counter))




(* Mutable Data structures

So far we have only considered immutable data structures such as lists
or trees, i.e. data structures that it is impossible to change the
structure of the list without building a modified copy of that
structure. Immutable data structures are persistent, i.e. operations
performed on them does not destroy the original structure. This
often makes our implementations easier to understand and reason
about. However, sometimes we do not want to rebuild our data
structure. A classic example is maintaining a dictionary. It is
clearly wasteful if we would need to carry around a large dictionary
and when we want to update it, we need to make a copy of it. This is
What we would like in this case is an "in place update"
operation. For this we must have {\em{ephemeral}} (opposite of
persistent) datastructures. We can achieve this by using references
in OCaml

Consider possibly circular lists. 

*)


(* Code for Reference Lists. *)

type 'a rlist = Empty | RCons of 'a * 'a rlist ref

type 'a refList = 'a rlist ref

(* Sometimes you want the tail as a reference, then use tail; 
sometimes you want the actual value, then use cdr .*)

let l1 = ref (RCons(4, ref Empty))
let l2 = ref (RCons(5, l1))

(* this will create a circular list *)
let _ =  l1 := !l2

(* Observe its output behavior 
   given a reference list l and a bound n
   observe(l,n) will print the first n elements.
 
   If we would not have this bound n, then
   observe would print repeatedly the elements in
   a circular list.
*) 
(* observe: 'a rlist * int -> unit *)
let rec observe l n = begin match l with
  | Empty ->  print_string "0"  
  | RCons(x, l) -> 
      if n = 0 then print_string "STOP\n"
      else (print_string (string_of_int x ^ " "); 
	    observe !l (n-1)) 
end


(* rapp : 'a refList * 'a refList -> unit *)
let rec rapp r1 r2 = begin match r1 with 
  | {contents = Empty} ->  r1 := !r2
  | {contents = RCons (h,t)} -> rapp t r2
end


(* This is a destructive reverse function.  *)
(* reverse : 'a refList -> ' a refList *)
let rev l0 = 
  let r = ref Empty in
  let rec rev' l = match !l with
    | Empty -> l0 := !r
    | RCons (h,t) -> 
      (r := RCons(h, ref (!r));
       rev' t)
  in
    (rev' l0; l0)
;;


(* ------------------------------------------------ *)

(* Mimicking object-oriented programming *)
let counter = ref 0 in 
let tick () = counter := !counter + 1; !counter in
let reset () = (counter := 0) in 
  (tick , reset)


(* This declaration introduces two functions tick:unit -> int
  and reset:unit -> unit. Their definitions share a private
    variable *counter* that is bound to a reference cell
    containing the current value of a shared counter. The tick
    operation increments the counter and returns its new value, and
    the reset operation resets its value to zero. The types
    already suggest that implicit state is involved. 
*)

(* Suppose we wish to have several different instances of a counter 
   and different pairs of functions tick and reset. We can achieve
   this by defining a counter generator 
*)
(* newCounter: unit -> {tick : unit -> int, reset: (unit -> unit)} *)
type counter_object = {tick : unit -> int ; reset: unit -> unit}

let newCounter () = 
  let counter = ref 0 in 
    {tick = (fun () -> counter := !counter + 1; !counter) ; 
     reset = fun () -> counter := 0}

(* We've packaged the two operations into a record containing
   two functions that share private state. There is an obvious
   analogy with class-based object-oriented programming. The function
   {\tt{newCounter}} may be thought of as a {\em{constructor}} for a
   class of counter {\em{objects}}. Each object has a private instance
   variable {\tt{counter}} that is shared between the methods
   {\tt{tick}} and {\tt{reset}}.

  Here is how to use counters.
*)

let c1 = newCounter()
let c2 = newCounter()

(* Notice, that c1 and c2 are distinct counters! *)


