
(*
  Unification.ml

  This file defines the unification

  Author : Luxon JEAN-PIERRE
*)

module T = IType

(* @note The function fails if a variable capture happened during the alpha-conversion *)

(*
  technically → (IVar, T.itype {IVar / real type})
  Sub → substitution
  Bottom → fail/occur-check
*)
type substitution = Sub of T.itype * T.itype | Bottom

(*The most greatest unifier *)
type unifier = substitution list

(* System to unify *)
type system = unifier


let compose f g x = f (g x)

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


let is_resolved : system -> bool = (fun x -> true)

let rec unify_aux (slist : system) : unifier = (*failwith "TODO unify"*)
  match slist with
  | [] -> []
  | _ ->
   (
    let res = process slist in
    if is_resolved res then res
    else failwith "unify: cannot infer the type of this expression"
   )

and process l = compose erase replace (compose swap decompose l)

and erase l = failwith "todo erase"

and replace l = failwith "todo replace"

and swap l = failwith "todo swap"

and decompose l = failwith "todo decomp"

let unify (slist : system) : unifier = (*failwith "TODO unify"*)
  match slist with
  | [] -> []
  | _ -> unify_aux slist
