
(*
  Conversion.ml

  This file handles alpha-conversion

  Author : Luxon JEAN-PIERRE
*)

open ChurchType;;

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
let rec alpha_conv e env : chexpression =
  match e with
  | Var(s) ->
    let ns = subs_var s env in
    Var(ns)

  | Pair(e1, e2) -> Pair((alpha_conv e1 env), (alpha_conv e2 env))

  | Apply(e1, e2) -> Apply((alpha_conv e1 env), (alpha_conv e2 env))

  | _ -> failwith "TODO the rest"

  and subs_var s env =
    match assoc_if s env with
    | None      -> s
    | Some(sub) -> sub
;;


let che = Pair ( Var("x") , Pair( Var("z"), Var("x") ) ) in
let env = ("x","y") :: ("z","w") :: [] in
let res = alpha_conv che env in
pretty_print_e res; print_endline("");;
