
(*
  Expression.ml

  Expression to infer the type of

  Author : Luxon JEAN-PIERRE
*)


type t =
    Var of string
  | Const of string
  | Pair of t * t
  | Apply of t * t
  | Lambda of string * t
  | Letin of string * t * t
