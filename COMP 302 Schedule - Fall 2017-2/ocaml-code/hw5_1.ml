(* Student information:

   Enter your name, and if you chose to work in pairs, the name of the
   student you worked with (both students MUST submit the solution to
   myCourses):

   Name: Yuhao Wu
   McGill ID: 260711365

   If you worked in pairs, the name of the other student.

   Name: Tianyi Wu
   McGill ID: 260714699

 *)

module Exp =
struct
  type name   = string
  type primop = Equals | LessThan | Plus | Minus | Times | Negate

  type exp =
    | Var of name
    | Int of int                      (* 0 | 1 | 2 | ... *)
    | Bool of bool                    (* true | false *)
    | If of exp * exp * exp           (* if e then e1 else e2 *)
    | Primop of primop * exp list     (* e1 <op> e2  or  <op> e *)
    | Let of dec * exp                (* let dec in e end  <--- NEW!!!! *)
    | Pair of exp * exp               (* (e1, e2)   <--- NEW!!!! *)
    | Fst of exp                      (* fst e <--- NEW!!!! *)
    | Snd of exp                      (* snd e <--- NEW!!!!*)


  and dec =
    | Val of exp * name               (* x = e *)
    | Match of exp * name * name      (* x, y = e  <--- NEW!!!! *)


  (* ---------------------------------------------------------------- *)
  (* Generating new variable names *)

  let genCounter =
  let counter = ref 0 in
  ((fun x ->
    let _ = counter := !counter+1 in
    x ^ string_of_int (!counter)),
  fun () ->
    counter := 0)

  let (freshVar, resetCtr) = genCounter

 (* ---------------------------------------------------------------- *)
 (* Basic functions about lists *)

  let member x l = List.exists (fun y -> y = x) l

  let rec delete (vlist, l) = match l with
    |  [] -> []
    |  h :: t ->
       if member h vlist then delete (vlist, t)
       else h :: delete (vlist, t)
 
  let rec union p = match p with
  | ([], l) -> l
  | (x::t, l) ->
    if member x l then
      union (t, l)
    else
      x :: union (t, l)

  (* ---------------------------------------------------------------- *)
  (* Computing the set of free variables in an expression *)

  (* Q1.2: extend the function for Pair(_, _) and Let (Match(_, _, _), _) *)

  let rec freeVars e = match e with
  | Var y -> [y]
  | Int n -> []
  | Bool b -> []
  | If(e, e1, e2) ->
    union (freeVars e, union (freeVars e1, freeVars e2))
  | Primop (po, args) ->
    List.fold_right (fun e1 fv -> union (freeVars e1, fv)) args []
  | Let (Val (e1, x), e2) ->
     union (freeVars e1, delete ([x], freeVars e2))
    
  | Pair (e1, e2) -> union(freeVars e1, freeVars e2)
  | Let (Match (e1, x, y), e2) ->union(freeVars e1, delete([x;y], freeVars e2))
(*  | Fst Var v ->[v]
  | Snd Var v ->[v]
(*  | Fst e -> freeVars e match e with 
             | Pair(e1, e2) -> freeVars e1
             | Var v -> [v] 
             | _ -> raise (Invalid_argument "Type Pair expected")
  | Snd e -> match e with
             | Pair(e1, e2) -> freeVars e2
              | Var v -> [v]
             | _ -> raise (Invalid_argument "Type Pair expected")
 *)
  | Fst Pair(e1, e2) -> freeVars e1
  | Snd Pair(e1, e2) -> freeVars e2
 *)
  | Fst e -> freeVars e
  | Snd e -> freeVars e

  (* ---------------------------------------------------------------- *)
  (* Substitution
   subst : (exp * name) -> exp -> exp

   subst (e',x) e = [e'/x]e

   subst replaces every occurrence of the variable x
   in the expression e with e'.
  *)

  (* Q1.4: extend subst for Pair(_, _) and both Let (Match(_,_,_), _)  and Let (Val(_,_), _) *)
  let rec subst (e',x as s) exp =
    match exp with
    | Var y ->
       if x = y then e'
       else Var y
    | Int n  -> Int n
    | Bool b -> Bool b
    | Primop(po, args) ->
       Primop(po, List.map (subst s) args)
    | If(e, e1, e2) ->
       If(subst s e, subst s e1, subst s e2)
    | Let (Val(e1,y), e2) ->
       let e1' = subst s e1 in
       if x = y then
         (* optimization: don't traverse e2 as there is not free occurrence of x in e2 *)
         Let (Val (e1', y), e2)
       else
         if member y (freeVars e') then
           let y'  = freshVar y in
           let e2' = rename (y', y) e2 in
             Let(Val(e1', y'), subst s e2')
         else
           Let(Val(e1', y), subst s e2)
    | Let (Match (e1, x, y), e2) -> 
       let e1' = subst s e1 in
       let x' = freshVar x in 
       let e2'  = rename(x',x) e2 in
       let y' = freshVar y in 
       let e2'' = rename (y',y) e2' in 
       Let (Match (e1', x', y'), subst s e2'') 
       
       
    | Pair (e1, e2) ->Pair(subst s e1, subst s e2)

    | Fst e -> Fst (subst s e)
    | Snd e -> Snd (subst s e)           

  and rename (x', x) e = subst (Var x', x) e
end

module Types =
  struct
    module E = Exp

    type tp = Int | Bool | Prod of tp * tp

    let rec typ_to_string t = match t with
      | Int -> "Int"
      | Bool -> "Bool"
      | Prod (t1, t2) -> typ_to_string t1 ^ " * " ^ typ_to_string t2

    exception TypeError of string

    let fail message = raise (TypeError message)

    type ctx = (E.name * tp ) list

    let lookup n g =
      try
        List.assoc n g
      with
        _ -> fail ("Could not find variable in the context")

    (* primopType p = (argTypes, returnType) *)
    let primopType p = match p with
      | E.Equals   -> ([Int; Int], Bool)
      | E.LessThan -> ([Int; Int], Bool)
      | E.Plus     -> ([Int; Int], Int)
      | E.Minus    -> ([Int; Int], Int)
      | E.Times    -> ([Int; Int], Int)
      | E.Negate   -> ([Int], Int)


    (* Q1.6: extend infer to support Pair(_, _) and Let (Match(_,_,_), _) *)

    let rec infer g e = match e with
      | E.Int _ -> Int
      | E.Bool _ -> Bool
      | E.If (e, e1, e2) ->
         (match infer g e with
         | Bool -> let t1 = infer g e1 in
                   let t2 = infer g e2 in
                   if t1 = t2 then t1
                   else fail ("Expected " ^ typ_to_string t1 ^
                              " - Inferred " ^ typ_to_string t2)
         | t -> fail ("Expected Bool\nInferred " ^ typ_to_string t))
      | E.Primop (po, args) ->
         let (expected_arg_types, resultType) = primopType po in
         let inferred_arg_types = List.map (infer g) args in

         let rec compare tlist1 tlist2 = match tlist1, tlist2 with
           | [] , [] -> resultType
           | t::tlist , s::slist ->
              if t = s then compare tlist slist
              else fail ("Expected " ^ typ_to_string t ^
                         " - Inferred " ^ typ_to_string s)
           | _ , _ -> fail ("Error: Primitve operator used with incorrect number of arguments")
         in
           compare expected_arg_types inferred_arg_types
      | E.Var x -> lookup x g
      | E.Let (E.Val (e1, x), e2) ->
         let t = infer g e1 in
         infer ((x, t)::g) e2

      | E.Pair (e1, e2) -> Prod(infer g e1, infer g e2)
      | E.Let (E.Match (e1, x, y), e2) -> match e1 with
                                          | Pair(x', y') -> let tx = infer g x' in 
                                                            let ty = infer g y' in 
                                                            infer ((x, tx)::(y, ty)::g) e2
                                          | _ -> fail ("Expected Pair")
    end

module Eval =
  struct
    open Exp

    exception Stuck of string

    (* Q1.8: extend eval to support Pair(_, _) and Let (Match(_,_,_), _) *)

    let evalOp op = match op with
      | (Equals,   [Int i; Int i']) -> Some (Bool (i = i'))
      | (LessThan, [Int i; Int i']) -> Some (Bool (i < i'))
      | (Plus,     [Int i; Int i']) -> Some (Int (i + i'))
      | (Minus,    [Int i; Int i']) -> Some (Int (i - i'))
      | (Times,    [Int i; Int i']) -> Some (Int (i * i'))
      | (Negate,   [Int i])         -> Some (Int (-i))
      | _                           -> None

    let rec eval e = match e with
      | Int _ -> e
      | Bool _ -> e
      | If(e, e1, e2) ->
         (match eval e with
         | Bool true -> eval e1
         | Bool false -> eval e2
         | _ -> raise (Stuck "guard is not a bool"))
  (* primitive operations +, -, *, <, = *)
      | Primop (po, args) ->
         let argvalues = List.map eval args in
         (match evalOp (po, argvalues) with
         | None -> raise (Stuck "Bad arguments to primitive operation")
         | Some v -> v)
      | Let (Val (e1, x), e2) -> eval (subst (eval e1, x) e2)
      | Var _ -> raise (Stuck "Bug : we only evaluate closed terms")    (* Variables would not occur in the evaluation of closed terms *)
      | Pair (e1, e2) -> Pair(eval e1, eval e2)
      | Let (Match (e1, x, y), e2) -> match e1 with
                                      |Pair(ex, ey) -> let x' = eval ex in
                                                       let y' = eval ey in  
                                                       let resultx = subst (x',x) e2 in
                                                         eval (subst (y',y) resultx)
                                      | _ -> raise (Stuck "Bug : We expect type Pair") 
  end


module E = Exp
let e1 = E.If (E.Primop (E.Equals, [E.Int 3; E.Int 2]),
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))

let e2 = E.If (E.Primop (E.Equals, [E.Int 3; E.Bool true]),
               E.Primop (E.Plus, [E.Int 5 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]),
               E.Primop (E.Plus, [E.Int 1 ; E.Primop (E.Times, [E.Int 3 ; E.Int 5])]))


let e3 = E.Let (E.Val (E.Int 3, "x"), E.Primop (E.Plus, [E.Var "x" ; E.Int 2]))

let e4 = E.Let (E.Val (E.Int 3, "x"),
                E.Let (E.Val (E.Int 2, "y"), E.Primop (E.Plus, [E.Var "x" ; E.Var "y"])))

   

(* Question 2 : There’s more than one way to do it  *)

(* Q2.1 Extend on the definition of the free variables function with fst and snd. *)
(* Q2.2 Extend the definition of subst function with fst and snd. *)


module type Optimization =
  sig
    val optimize : E.exp -> E.exp
  end

(* Q3.1: implement dead code elimintion *)

module DeadCode : Optimization =
struct
  open Exp

  exception Unmatched of string

  let rec memofarg var args = match args with
    | [] -> false 
    | (Int h) :: t -> memofarg var t 
    | (Var h) :: t -> if h = var then true else memofarg var t
    | (Fst Var h) :: t -> if h = var then true else memofarg var t
    | (Snd Var h) :: t -> if h = var then true else memofarg var t
    | (Primop(po, args')) :: t -> (memofarg var args') || (memofarg var t) 
    | h :: t ->  raise (Unmatched "int or variable required")

           
  let rec optimize e =  match e with
    |Int _ -> e 
    |Bool _ -> e
    |Var _ -> e 
    |If(e0, e1, e2) -> If(optimize e, optimize e1, optimize e2)
    |Primop _ -> e
    |Let (Val (e1, x), e2) ->
      (
        match e2 with
        |Int _ -> e2
        |Bool _ -> e2
        |Var v -> if v = x
                  then Let(Val (optimize e1, x), e2) 
                  else e2 
        |If(e0', e1', e2') ->
          if ( (member x (freeVars e0') ) || (member x (freeVars e1')) || (member x (freeVars e2')))
          then (Let(Val(optimize e1, x), optimize e2))
          else optimize e2
        |Primop(po, args) ->
          if memofarg x args
          then Let(Val (optimize e1, x), e2)
          else e2
        |Let(Val (e1', x'), e2') ->
          let opt = optimize e2 in
          if member x (freeVars opt)
          then Let(Val(optimize e1, x), opt)
          else opt
        |Pair (e1', e2') ->
          let opte1 = optimize e1' in 
          let opte2 = optimize e2' in 
          if ((member x (freeVars opte1)) || (member x (freeVars opte2)))
          then Let(Val (optimize e1, x), Pair(opte1, opte2))
          else Pair(opte1, opte2)
        |Let (Match (e1', x', y'), e2') ->
          let opt = optimize e2 in
          if ((member x (freeVars opt)))
          then Let(Val (optimize e1, x), opt)
          else opt
        |Fst e' ->
          let e' = optimize e' in 
           (match e' with 
            | Var v ->
               if v = x then Let(Val (optimize e1, x), Fst e')
               else Fst e' 
            | Pair(a,b) -> if (member x (freeVars e'))
                           then Let(Val (optimize e1, x), Fst e')
                           else Fst e'
            | _ -> raise (Unmatched  "Type Pair expected(2)")
           )

        |Snd e' -> let e' = optimize e' in 
                   (match e' with 
                    | Var v -> if v = x
                               then Let(Val (optimize e1, x), Snd e')
                               else Snd e' 
                    | Pair(a,b) -> if (member x (freeVars e'))
                                   then Let(Val (optimize e1, x), Snd e')
                                   else Snd e'
                    | _ -> raise (Unmatched  "Type Pair expected(3)")
                   )         
      )      

    |Pair (e1, e2) -> Pair(optimize e1, optimize e2)
     
     
    |Let (Match (e1, x, y), e2) ->
(
      match e2 with
      |Int _ -> e2
      |Bool _ -> e2
      |Var v -> if ((v = x) || (v = y))
                then Let(Match ((optimize e1), x, y), e2) 
                else e2 
      |If(e0', e1', e2') ->
        if ( (member x (freeVars e2) ) || (member y (freeVars e2)))
        then (Let(Match (optimize e1, x, y), optimize e2))
        else optimize e2
      |Primop(po, args) -> if(  (memofarg x args)||(memofarg y args) )
                           then Let(Match (optimize e1, x, y), e2)
                           else e2
      |Let(Val (e1', x'), e2') -> let opt = optimize e2 in
                                  if ((member x (freeVars opt)) || (member y (freeVars opt)))
                                  then Let(Match(optimize e1, x, y), opt)
                                  else opt
      |Pair (e1', e2') -> let opte1 = optimize e1' in 
                          let opte2 = optimize e2' in
                          let newexp = Pair(opte1, opte2) in  
                          if ((member x (freeVars newexp)) || (member y (freeVars newexp)))
                          then Let(Match (optimize e1, x, y), newexp)
                          else newexp
      |Let (Match (e1', x', y'), e2') -> let opt = optimize e2 in
                                         if ((member x (freeVars opt)) || (member y (freeVars opt)))
                                         then Let(Match(optimize e1, x, y), opt)
                                         else opt
      |Fst e' -> let e' = optimize e' in 
                 ( match e' with 
                   | Var v -> if ((v = x)||(v = y))
                              then Let(Match (optimize e1, x, y), Fst e')
                              else Fst e'
                   | Pair(a,b) -> if ((member x (freeVars e'))||(member y (freeVars e')))
                                  then Let(Match (optimize e1, x, y), Fst e')
                                  else Fst e'
                   | _ -> raise (Unmatched  "Type Pair expected")
                 )
                 

            
      |Snd e' -> let e' = optimize e' in 
                 ( match e' with 
                   | Var v -> if ((v = x)||(v = y))
                              then Let(Match (optimize e1, x, y), Snd e')
                              else Snd e'
                   | Pair(a,b) -> if ((member x (freeVars e'))||(member y (freeVars e')))
                                  then Let(Match (optimize e1, x, y), Snd e')
                                  else Snd e'
                   | _ -> raise (Unmatched  "Type Pair expected")
                 )
             
)        
    |Fst e' -> Fst (optimize e')
    |Snd e' -> Snd (optimize e')
end



(* Q3.2: implement the elimination of pattern matching let *)
module RemoveLetMatch : Optimization =
  struct
    open Exp

    exception Unmatched of string
                         
    let rec optimize e =  match e with
      |Pair (e1, e2) ->  Pair(optimize e1, optimize e2)
                       
      |Let (Val (e1, x), e2) -> Let(Val (optimize e1, x),  optimize e2)
                              
      |If(e0', e1', e2') -> If(optimize e0',optimize e1',optimize e2') 
                          
      |Let (Match (e1, x, y), e2) ->
        (match e2 with
         |Var v -> if v = x then Let(Val(e1, x), Fst (Var x) ) 
                   else if v = y then Let(Val(e1, y), Snd (Var y) )
                   else Let(Val (e1, x), e2)
                 
         |If(e0', e1', e2') ->let newe2 = optimize e2 in 
                              let h = subst (Fst (Var x), x) newe2 in
                              Let(Val(e1, x), subst ( Snd(Var x), y) h)
                               
         |Primop(po, args) ->  let h = subst (Fst(Var x), x) e2 in
                               Let(Val(e1, x), subst (Snd (Var x), y) h)
                               
         |Let(Val (e1', x'), e2') -> let newe2 = optimize e2 in
                                     let h = subst (Fst (Var x), x) newe2 in
                                     Let(Val(e1, x), subst (Snd (Var x), y) h)
                                     
         |Pair(e1, e2) -> let h1 = subst (Fst (Var x), x) e1 in
                          let h1' = subst (Snd (Var x), y) h1 in
                          let h2 = subst (Fst (Var x), x) e2 in
                          let h2' = subst(Snd (Var x), y) h2 in
                          Let(Val(e1, x), Pair(h1', h2'))
                          
         |Let (Match (e1', x', y'), e2') -> let opt = optimize e2 in
                                            let h1 = subst (Fst(Var x), x) opt in
                                            let h1' = subst (Snd (Var x), y) h1 in
                                            Let(Val(e1, x), h1')
         |Fst e' -> (match e' with
                     | Var v -> e
                     | Pair (e1,e2) ->  let h1 = subst (Fst (Var x), x) e1 in
                                        let h1' = subst (Snd (Var x), y) h1 in
                                        let h2 = subst (Fst (Var x), x) e2 in
                                        let h2' = subst(Snd (Var x), y) h2 in
                                        Let(Val(e1, x), Fst (Pair (h1', h2')))
                     | _ -> raise (Unmatched  "Type Pair expected") 
                    )
                  
         |Snd e' -> (match e' with
                     | Var v -> e
                     | Pair (e1,e2) ->  let h1 = subst (Fst (Var x), x) e1 in
                                        let h1' = subst (Snd (Var x), y) h1 in
                                        let h2 = subst (Fst (Var x), x) e2 in
                                        let h2' = subst(Snd (Var x), y) h2 in
                                        Let(Val(e1, x), Snd (Pair(h1', h2')))
                     |  _ -> raise (Unmatched  "Type Pair expected")
                    )
         |_ -> e
             
        )                    
      |_ -> e
end

  

module Compose (M1 : Optimization) (M2 : Optimization) : Optimization =
  struct
    let optimize e = M1.optimize (M2.optimize e)
  end

(* To test one after the other use this pipeline *)
(* module Pipeline = Compose (DeadCode) (RemoveLetMatch) *)
(* Think about if the order in which you apply matters? is this always
   the case? is there always a good choice? *)


(*

let x,y = 5,7 in let z = 2 in x + y +z => 14

let x,y = 5,7 in let z = 2 in y +z => 9

let x,y = 5,7 in let z = 2 in x + y  => 12

*)

let plus e1 e2 =
  Exp.Primop(Exp.Plus, [e1 ; e2])

(* These lines use a local opening of modules, check this URL to see the explanation:
   https://realworldocaml.org/v1/en/html/files-modules-and-programs.html
*)
let e5 = let open Exp in Let (Match (Pair (Int 5, Int 7), "x", "y"), Let (Val (Int 2, "z"), plus (Var "x") (plus (Var "y") (Var "z"))))
let e6 = let open Exp in Let (Match (Pair (Int 5, Int 7), "x", "y"), Let (Val (Int 2, "z"), plus (Var "y") (Var "z")))
let e7 = let open Exp in Let (Match (Pair (Int 5, Int 7), "x", "y"), Let (Val (Int 2, "z"), plus (Var "x") (Var "y")))


let e8 = let open Exp in Let (Val (Int 3, "z"), Let (Val (Int 7, "x"), Let (Val (Var "x", "y"), Var "z")))
