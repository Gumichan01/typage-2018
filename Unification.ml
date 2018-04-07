
(*
  Unification.ml

  This file defines the unification

  Author : Luxon JEAN-PIERRE
*)

module T = IType

(* @note The function fails if a variable capture happened during the alpha-conversion *)

(*
  technically → (IVar, T.itype {IVar / real type})
*)
type substitution = T.itype * T.itype

(*The most greatest unifier *)
type unifier = substitution list

(* System to unify *)
type system = unifier

exception RecursiveType
exception TypeMismatch
exception OccursCheck
exception Conflict


let delete tsl =
  let rec aux_delete l res =
    match l with
    | [] -> res
    | (s1, s2)::q when s1 = s2 -> aux_delete q (res)
    | h::q -> aux_delete q (h::res)
  in aux_delete tsl []

let swap tsl =
  let rec aux_swap l res =
    match l with
    | [] -> res
    | (T.IBool, T.IVar(s))::q -> aux_swap q ((T.IVar(s), T.IBool)::res)
    | (T.IInt, T.IVar(s))::q  -> aux_swap q ((T.IVar(s), T.IInt)::res)
    | h::q -> aux_swap q (h::res)
  in aux_swap tsl []


let is_variable = function
  | T.IVar(_) -> true
  | _ -> false

(*
    Checks if a variable belongs to the variables of a term
*)
let rec vars alpha = function
  | T.IVar(_) as a when alpha = a -> true
  | T.IVar(_) | T.IInt | T.IBool  -> false
  | T.ICross(m, n) -> (vars alpha m) || (vars alpha n)
  | T.IArrow(a, b) -> (vars alpha a) || (vars alpha b)

(*
    Checks if a variable is in a system
*)
let rec varsl alpha = function
  | []   -> false
  | (a, b)::q ->
    begin
      match (vars alpha a), (vars alpha b) with
      | true, _
      | _ , true  -> true
      | _ -> varsl alpha q
    end


let rec sub ((a, t) : substitution) = function
  | x when x = a   -> t
  | T.ICross(m, n)  -> T.ICross( (sub (a,t) m), (sub (a,t) n) )
  | T.IArrow(m, n)  -> T.IArrow( (sub (a,t) m), (sub (a,t) n) )
  | _ as i -> i

let substitute (s: substitution) (t1, t2) = (sub s t1, sub s t2)

let substitute_all (s: substitution) (l : system) = (List.map (substitute s) l : system)

let rec system_without_subs (s: substitution) : system -> system = function
  | [] -> []
  | h::q when h = s -> q
  | h::q -> h :: (system_without_subs s q)


let is_resolved : system -> bool = (fun x -> true)

let rec unify_aux (slist : system) : unifier = (*failwith "TODO unify"*)
  match slist with
  | [] -> []
  | _ ->
   begin
     let res = process slist in
     if is_resolved res then
       res
     else
       failwith "unify: cannot infer the type of this expression"
   end

and process l = List.map check ( l |> decompose |> swap |> eliminate |> erase )

and erase l = l(*failwith "todo erase"*)

and eliminate l = l (*eliminate_aux l l*)

and eliminate_aux g = function
  | [] -> []
  | (a, t)::q when (is_variable a) ->
    begin
      if not(vars a t) && (varsl a g) then
        begin
          let s = (a, t) in
          let ng = system_without_subs s g in (* E { a ← t } *)
           eliminate ( s :: (substitute_all s ng) ) (* E' U { a = t } *)
        end
      else
        (a, t) :: (eliminate_aux g q)
    end
  | h::q -> h :: (eliminate_aux g q)

and swap l = l(*failwith "todo swap"*)

and decompose = function
  | [] -> []    (* yes, it is possible *)
  | (T.ICross(u, w), T.ICross(v, x))::q
  | (T.IArrow(u, w), T.IArrow(v, x))::q -> (u, v) :: (w, x) :: (decompose q)
  | h::q -> h :: (decompose q)

(*
    Check if a substitution is not ill-formed
    -> occurs check and conflict
*)
and check p =
  if occurs_check p then
    raise OccursCheck
  else
    if conflict p then
      raise Conflict
    else p


and occurs_check = function
  | (T.IVar(s), T.ICross(_, _))
  | (T.IVar(s), T.IArrow(_, _))-> true (* TODO: check that *)
  | _ -> false

(*
    f(s₀, ..., sₖ) = g(t₀, ..., tₙ)     f != g or k != n
*)
and conflict = function
  | (T.IInt, T.ICross(_, _)) | (T.ICross(_, _), T.IInt)
  | (T.IInt, T.IArrow(_, _)) | (T.IArrow(_, _), T.IInt)
  | (T.IBool, T.ICross(_, _)) | (T.ICross(_, _), T.IBool)
  | (T.IBool, T.IArrow(_, _)) | (T.IArrow(_, _), T.IBool) -> true
  | _ -> false


let unify (slist : system) : unifier =
  match slist with
  | [] -> []
  | _ -> unify_aux slist


(* Just to test *)

let rec to_string = function
  | T.IInt -> "int"
  | T.IBool -> "bool"
  | T.ICross(x, y) -> (to_string x) ^ " × " ^ (to_string y)
  | T.IArrow(x, y) -> "(" ^ (to_string x) ^ ") → (" ^ (to_string y) ^ ")"
  | T.IVar(s) -> s


let printI (a, b) =
  print_string((to_string a) ^ "/" ^ (to_string b));
  print_endline("");;


let s =
[ (T.IVar("α1"), T.IInt);
  (T.IInt, T.IInt);
  (T.IBool, T.IVar("α2"));
  (T.IVar("α1"), T.IVar("α4"));
  (T.ICross(T.IVar("α3"), T.IVar("α4")), T.ICross(T.IBool, T.IInt)) ] in
let res = unify s in
List.map printI res;
