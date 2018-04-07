
(*
  Unification.ml

  This file defines the unification

  Author : Luxon JEAN-PIERRE
*)

module T = IType

(* @note The function fails if a variable capture happened during the alpha-conversion *)

let rec unify_all (tslist : (T.itype * T.itype) list) : unit =

  match tslist  with
  | [] -> print_string("done") (* todo return something? *)
  | h::q -> print_string(" todo ..."); unify_all ((unify)::q)

and unify = failwith "TODO unify"


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
