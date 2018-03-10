
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


(*let rec unify (tslist : (typeSchema * typeSchema) list) : (typeSchema * typeSchema) list =
  let aux_unify l res =
    match l with
    | [] -> res
    | h::q -> print_string(" todo ..."); unify q
  in aux_unify tslist []*)


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


(*
and operate : (typeSchema * typeSchema) -> unit =
  function
  | (a,a) ->
  | _ -> expr2*)
