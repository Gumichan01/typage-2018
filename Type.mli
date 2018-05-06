
(*
  Type.mli

  Type system + inference

  Author : Luxon JEAN-PIERRE
*)

type itype =
    Bool
  | Int
  | Cross of itype * itype
  | Arrow of itype * itype
  | Tvar of string

val to_string : itype -> string

(* This module can generate a fresh variable (α) for the type inference *)
module V : sig type t = string val create : unit -> itype end

(*
  Generalization of a type
  Used in Walgo.infer
*)
val gen_type : itype -> itype
