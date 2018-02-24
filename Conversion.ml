
(*
  Conversion.ml

  This file handles alpha-conversion

  Author : Luxon JEAN-PIERRE
*)

open ChurchType

(*
  A substitution is just a pair <old_name, new_name> that represents {x / y}
  *)
type substitution = string * string;;

type subList = substitution list;;


let assoc_if a l =
  try
      Some(List.assoc a l)
  with
  | _ -> None

(*
  todo
  It operates the alpha-conversion by taking a church type and a substitution list
  to apply

  @note The function fails if a variable capture happened during the alpha-conversion
*)
let rec alpha_conv e env : ChurchType.chexpression =
  match e with
  | ChurchType.Var(s) ->
    let ns = subs_var s env in
    ChurchType.Var(ns)
  | _ -> failwith "TODO the rest"

  and subs_var s env =
    match assoc_if s env with
    | None      -> s
    | Some(sub) -> sub
