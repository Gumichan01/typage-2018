
(*
  Walgo.ml

  This file implements the W-algorithm

  Author : Luxon JEAN-PIERRE
*)

module T = Type
module E = Expression
module U = Unification

module V = struct
  type t = string
  let compare v1 v2 = Pervasives.compare v1 v2
  let equal v1 v2 = v1 = v2
  let create = let r = ref 0 in fun () -> incr r; T.Tvar("α" ^ string_of_int(!r))
end

let math_ops = ["+"; "-"; "*"; "/"]
let bool_ops = ["&&"; "||"]
let math_basictype = T.Arrow(T.Cross(T.Int, T.Int), T.Int)
let bool_basictype = T.Arrow(T.Cross(T.Bool, T.Bool), T.Bool)

type expression = E.t

(*
  There is a function that does the job in Ocaml 4.06, but I don't have this version.
  So I implemented it myself
*)
let lassoc_opt e l =
  try
      Some(List.assoc e l)
  with
  | _ -> None


let from_sblist sblist =
  List.map (fun ( U.Sub( a, b ) ) -> ( a, b ) ) sblist


(* Typing environment *)
type environment = (string * T.itype) list

(* substitution *)
(*type unifier = (T.itype * T.itype) list*)
type unifier = U.unifier

(* unifier composition *)
let cunifier u1 u2 = U.compose_unifier ( U.Unifier(u1) ) ( U.Unifier(u2) )

(*
    Substitute a type by replacing it with another type, if it can be replaced
    returns the argument itself otherwise
 *)
let apply_r rho a =
  match ( lassoc_opt a ( from_sblist rho ) ) with
  | Some(t) -> t
  | None -> a

(*
    W-algorithm
*)
let rec infer (delta : environment) (e : expression) =
  match e with
  | E.Var(_) | E.Const(_) as cv -> ( inst delta cv, [] )
  | E.Pair(n, l) ->
    let b, rhob = infer delta n in
    let c, rhoc = infer ( sigma delta rhob ) l in
    ( T.Cross( (apply_r rhoc b), c ), ( cunifier rhob rhoc ) )

  | E.Apply(n, l) -> (*failwith "TODO W-algorithm: Apply"*)
    let b, rhob = infer delta n in
    let c, rhoc = infer ( sigma delta rhob ) l in
    let alpha = ( V.create () ) in
    let eql = [ U.Eq( (apply_r rhoc b), T.Arrow( c, alpha ) ) ] in
    (* for debug
    let printE (U.Eq(a, b)) = print_endline ( (T.to_string a) ^ " = " ^ (T.to_string b) ) in
    print_endline ("========"); ignore (List.map printE eql); print_endline ("========");
    for debug - end *)
    let (U.Unifier(mgu)) = (*U.unify ( U.from_eql [] ) in*) U.unify ( U.from_eql eql ) in
    ( (apply_r mgu alpha), cunifier ( cunifier rhob rhoc ) mgu )

  | E.Lambda(x, n) -> (*failwith "TODO W-algorithm: Lambda"*)
    let fresh_alpha =  ( V.create () ) in
    let b, rho = infer ( ( x, fresh_alpha )::delta ) n in
    ( T.Arrow( ( apply_r rho fresh_alpha), b ), rho ) (* change it *)

  | E.Letin(_,_,_) -> failwith "TODO W-algorithm: Letin"

  (*
    I want to make the following calculation

    Let Γ = x₁ : A₁, ..., xₙ : Aₙ an environment, and σ a type substitution.
    So σ(Γ) = x₁ : σ(A₁), ..., xₙ : σ(Aₙ)

  *)
  and sigma (delta: environment) sub =
    match sub with
    | [] -> delta
    | _ ->
      begin
          match delta with
          | [] -> delta
          | _ ->  sigma_in delta sub
      end

  (* pre-condition: sub is not an empty list *)
  and sigma_in (delta: environment) sub =
    match delta with
    | [] -> delta
    | (x, a)::q ->
      begin
        match ( lassoc_opt a ( from_sblist sub ) ) with
        | Some(t) -> (x, t) :: sigma_in q sub
        | None -> (x, a) :: sigma_in q sub
      end

  (* Get the type instance of the variable or the constant value *)
  and inst env = function
  | E.Var(s)   -> List.assoc s env
  | E.Const(x) -> inst_constv env x
  | _-> assert(false) (* type instance *)


  (* Get the type instance of constant value *)
  and inst_constv env x =
    match inst_intv x with
     | Some(_) -> T.Int
     | None ->
       begin
           match x with
           | "true" | "false" -> T.Bool
           | "+" | "-" | "*" | "/" -> T.Arrow( T.Cross( T.Int, T.Int ), T.Int )
           | "fst" ->
             begin
               let a1 = ( V.create () ) in
               let a2 = ( V.create () ) in
               T.Arrow( T.Cross( a1, a2 ), a1 )
             end

           | "snd" ->
             begin
               let a1 = ( V.create () ) in
               let a2 = ( V.create () ) in
               T.Arrow( T.Cross( a1, a2 ), a2 )
             end

           | "ifthenelse" ->
              begin
                let a = ( V.create () ) in
                T.Arrow( T.Cross( T.Bool, T.Cross( a, a) ), a )
              end

           | "fix" ->
             begin
               let a = ( V.create () ) in
               T.Arrow( T.Cross( a, a), a )
             end

           | _ -> assert(false) (* pre-condition: integer or boolean value *)
       end

  and inst_intv s =
    try
      Some(int_of_string s)
    with
    | _ -> None
;;


(* just to test *)
let eval expr : unit =
  let ty, _ = infer [] expr in
  print_endline ( T.to_string ty );;


(* tests *)

(* constant values *)
print_endline ("> 42");;
eval ( E.Const("42") );;

print_endline ("\n> true and false");;
eval ( E.Const("true") );;
eval ( E.Const("false") );;

print_endline ("\n> + | fst | snd | ifthenelse | fix");;
eval ( E.Const("+") );;
eval ( E.Const("fst") );;
eval ( E.Const("snd") );;
eval ( E.Const("ifthenelse") );;
eval ( E.Const("fix") );;

(* pairs *)
print_endline ("\n> pairs");;
let p1 = E.Pair( E.Const("42"), E.Const("42") );;
let p2 = E.Pair( E.Const("42"), E.Const("true"));;
eval ( p1 );;
eval ( p2 );;
eval ( E.Pair( E.Const("64"), E.Pair( E.Const("42"), p2 ) ) );;
eval ( E.Pair( E.Pair( E.Const("false"), E.Const("true") ), p1 ) );;

(* lambda *)
print_endline ("\n> λx. x + x");;
eval ( E.Lambda( "x", E.Apply( E.Const("+"), E.Pair( E.Var("x"), E.Var("x") ) ) ) );;

print_endline ("\n> (λx. x + x) 42");;
eval ( E.Apply( E.Lambda( "x", E.Apply( E.Const("+"), E.Pair( E.Var("x"), E.Var("x") ) ) ), E.Const("42") ) );;

(* identity *)
print_endline ("\n> (λx.x)");;
eval ( E.Lambda( "x", E.Var("x") ) );;

print_endline ("\n> (λx.x) 42");;
eval ( E.Apply( E.Lambda( "x", E.Var("x") ), E.Const("42") ) );;

let id_fun = E.Apply( E.Lambda( "x", E.Var("x") ),
                      E.Lambda( "y", E.Apply( E.Const("+"),
                                              E.Pair( E.Var("y"), E.Var("y") ) ) ) ) in

print_endline ("\n> (λx.x) (λy. y + y)");
eval ( id_fun );
print_endline ("\n> (λx.x) (λy. y + y) 42");
eval ( E.Apply( id_fun, E.Const("42") ) );;

(* addition *)
print_endline ("\n> λx.λy. x + y ");;
let fplus       = E.Apply( E.Const("+"), E.Pair( E.Var("x"), E.Var("y") ) ) in
let fsum        = E.Lambda( "x", ( E.Lambda( "y", fplus ) ) ) in
let partial_sum = E.Apply( fsum, E.Const("42") ) in
eval ( fsum );
print_endline ("\n> (λx.λy. x + y ) 42");
eval ( partial_sum );
print_endline ("\n> (λx.λy. x + y ) 42 1");
eval ( E.Apply( partial_sum , E.Const("1") ) );;

(* if-then-else *)
let one = E.Const("1") in
let two = E.Const("2") in
let autoplus = E.Lambda( "x", E.Apply( E.Const("+"), E.Pair( E.Var("x"), E.Var("x") ) ) ) in
let apif = E.Apply( E.Const("ifthenelse") , E.Pair( E.Const("true") , E.Pair( one, two ) ) ) in

print_endline ("\n> if-then-else ");
eval ( E.Const("ifthenelse") );
print_endline ("\n> if true then 1 else 2");
eval ( apif );
print_endline ("\n> (if true then 1 else 2) + 1");
eval ( E.Apply( autoplus, apif ) );;

(* factorial *)

(*
    Comment:

    - (DOING final goal) Apply the algorithm for each element of type chtype (an expression).
      (DONE) apply_r a type by another using the substitution
      (DONE) σ₁ o σ₂ function
    - (DONE) Unification
    - (DONE) Free and bound variables

*)
