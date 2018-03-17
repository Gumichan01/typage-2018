
(*
  Unification.ml

  This file defines the unification

  Author : Luxon JEAN-PIERRE
*)

type typeSchema =
   Alpha of string
  | Bool
  | Int
  | Arrow of typeSchema * typeSchema
  | Cross of typeSchema * typeSchema

(* @note The function fails if a variable capture happened during the alpha-conversion *)


let rec unify_all (tslist : (typeSchema * typeSchema) list) : unit =
  match tslist  with
  | [] -> print_string("done") (* todo return something? *)
  | h::q -> print_string(" todo ..."); unify_all ((unify)::q)

and unify =
  function
  | _ -> failwith "TODO unify"


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
    | (Bool, Alpha(s))::q -> aux_swap q ((Alpha(s), Bool)::res)
    | (Int, Alpha(s))::q  -> aux_swap q ((Alpha(s), Int)::res)
    | h::q -> aux_swap q (h::res)
  in aux_swap tsl []
