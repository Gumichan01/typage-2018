
(*
  IType.ml

  Type to infer

  Author : Luxon JEAN-PIERRE
*)


type itype =
  | Bool
  | Int
  | Cross of itype * itype
  | Arrow of itype * itype
  (* temporay type I am using *)
  | Tvar of string
