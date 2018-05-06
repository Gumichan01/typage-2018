(*
  Expression.mli

  Expression (Curry)

  Author : Luxon JEAN-PIERRE
*)

type t =
    Var of string
  | Const of string
  | Pair of t * t
  | Apply of t * t
  | Lambda of string * t
  | Letin of string * t * t

(*
 Const may be:
 - an integer
 - a boolean value
 - a built-in arithmetical operator
 - a built-in comparison operator
 - boolean operator
 - ifthenelse
 - fst, snd, fix
*)
