
(*
  Unification.ml

  This file defines the unification

  Author : Luxon JEAN-PIERRE
*)

module T = Type

(* @note The function fails if a variable capture happened during the alpha-conversion *)

(*
  technically → (Tvar, T.itype {Tvar / real type})
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


let is_variable = function
  | T.Tvar(_) -> true
  | _ -> false

(*
    Checks if a variable belongs to the variables of a term
*)
let rec vars alpha = function
  | T.Tvar(_) as a when alpha = a -> true
  | T.Tvar(_) | T.Int | T.Bool  -> false
  | T.Cross(m, n) -> (vars alpha m) || (vars alpha n)
  | T.Arrow(a, b) -> (vars alpha a) || (vars alpha b)

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
  | T.Cross(m, n)  -> T.Cross( (sub (a,t) m), (sub (a,t) n) )
  | T.Arrow(m, n)  -> T.Arrow( (sub (a,t) m), (sub (a,t) n) )
  | _ as i -> i

let substitute (s: substitution) (t1, t2) = (sub s t1, sub s t2)

let substitute_all (s: substitution) (l : system) = (List.map (substitute s) l : system)

let rec system_without_subs (s: substitution) : system -> system = function
  | [] -> []
  | h::q when h = s -> q
  | h::q -> h :: (system_without_subs s q)

(*
    pre-condition:  { α₁ = t₁, ..., αₙ = tₙ }

    Every α are variables. t could be anything
*)
let distinct (l : system) =
  let rec d_aux l hashtbl =
    match l with
    | [] -> true
    | (T.Tvar(a), _)::q ->
      begin
        if Hashtbl.mem hashtbl a then
          false
        else
          begin
            Hashtbl.add hashtbl a 0;
            d_aux q hashtbl
          end
      end
    | _ -> assert false (* pre-condition *)
  in d_aux l ( Hashtbl.create (List.length l) )

let is_resolved : system -> bool = (*(fun x -> true)*)
  (fun l -> distinct l)


(** The function that makes the unification *)

let unify (slist : system) : unifier =
  let rec unify_aux (sys : system) : unifier =
    begin
      let nsys = process sys in
      if is_resolved nsys then
        nsys
      else
        unify_aux nsys
    end

  and process l = List.map check ( l |> decompose |> swap |> eliminate |> erase )

  and erase l =
    let rec aux_erase sl res =
      match sl with
      | [] -> res
      | (s1, s2)::q when s1 = s2 -> aux_erase q (res)
      | h::q -> aux_erase q (h::res)
    in aux_erase l []

  and eliminate l = eliminate_aux l l

  and eliminate_aux g = function
    | [] -> []
    | (a, t)::q when (is_variable a) ->
      begin
        let s = (a, t) in
        let ng = system_without_subs s g in (* E { a ← t } *)
        if not(vars a t) && (varsl a ng) then
          begin
            eliminate ( s :: (substitute_all s ng) ) (* E' U { a = t } *)
          end
        else
          (a, t) :: (eliminate_aux g q)
      end
    | h::q -> h :: (eliminate_aux g q)

  and swap l =
    let rec aux_swap sl res =
      match sl with
      | [] -> res
      | ( T.Tvar(s), x )::q
      | ( x, T.Tvar(s) )::q -> aux_swap q ( (T.Tvar(s), x)::res )
      | h::q -> aux_swap q (h::res)
    in aux_swap l []

  and decompose = function
    | [] -> []    (* yes, it is possible *)
    | (T.Cross(u, w), T.Cross(v, x))::q
    | (T.Arrow(u, w), T.Arrow(v, x))::q -> (u, v) :: (w, x) :: (decompose q)
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
    | (T.Tvar(s), T.Cross(x, y))
    | (T.Tvar(s), T.Arrow(x, y)) ->
      begin
          let a = T.Tvar(s) in
          vars a x && vars a y
      end
    | _ -> false

  (*
      . f(s₀, ..., sₖ) = g(t₀, ..., tₙ)     f != g or k != n
      . int = bool
      . bool = int
      . α × α = α -> α
      . α = α × α  - recursive type
      . α = α -> α - recursive type
  *)
  and conflict = function
    | (T.Int, T.Bool) | (T.Bool, T.Int)
    | (T.Int, T.Cross(_, _)) | (T.Cross(_, _), T.Int)
    | (T.Int, T.Arrow(_, _)) | (T.Arrow(_, _), T.Int)
    | (T.Bool, T.Cross(_, _)) | (T.Cross(_, _), T.Bool)
    | (T.Bool, T.Arrow(_, _)) | (T.Arrow(_, _), T.Bool) -> true
    | _ -> false

  in unify_aux slist


(* Just to test *)

let rec to_string = function
  | T.Int -> "int"
  | T.Bool -> "bool"
  | T.Cross(x, y) -> (to_string x) ^ " × " ^ (to_string y)
  | T.Arrow(x, y) -> "(" ^ (to_string x) ^ ") → (" ^ (to_string y) ^ ")"
  | T.Tvar(s) -> s


let printI (a, b) =
  print_string((to_string a) ^ "/" ^ (to_string b));
  print_endline("");;


let s =
[ (T.Tvar("α1"), T.Int);
  (T.Int, T.Int);
  (T.Bool, T.Tvar("α2"));
  (T.Tvar("α1"), T.Tvar("α4"));
  (T.Cross(T.Tvar("α3"), T.Tvar("α4")), T.Cross(T.Bool, T.Int)) ] in
let res = unify s in
List.map printI res;
