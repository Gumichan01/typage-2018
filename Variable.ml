
(*
  Variable.ml

  Author : Luxon JEAN-PIERRE
*)

(** TODO create a file misc(*elleanous*).ml and free_variable, bound_variable,
    and alpha-conversion in there -> necessary? *)

(*
  A substitution is just a pair <old_name, new_name> that represents {x / y}
  *)

module E = Expression

(* miscelleanous function *)
let assoc_if a l =
  try
      Some(List.assoc a l)
  with
  | _ -> None

(*
  Generate the list of free variables
*)
let free_variable chexpr =
  let rec aux_fv bvl = function
  | E.Var(s) ->
    (
        match (List.mem s bvl) with
        | true  -> []
        | false -> [s]
    )
  | E.Const(_) -> []
  | E.Pair(m, n)  -> (aux_fv bvl m) @ (aux_fv bvl n)
  | E.Apply(m, n) -> (aux_fv bvl m) @ (aux_fv bvl n)
  | E.Lambda(x,_, m) -> (aux_fv (x::bvl) m)
  | E.Letin(x,_, m, n)  -> (aux_fv (x::bvl) m) @ (aux_fv (x::bvl) n)
  in aux_fv [] chexpr

(*
  Generate the list of bound variables
*)
let rec bound_variable = function
  | E.Var(_)
  | E.Const(_) -> []
  | E.Pair(m, n)
  | E.Apply(m, n) -> (bound_variable m) @ (bound_variable n)
  | E.Lambda(x,_, m) -> (bound_variable m) @ [x] (* @ (bound_variable n) *)
  | E.Letin(x,_, m, n)  -> (bound_variable m) @ (bound_variable n) @ [x]

(*
  TODO
  It operates the alpha-conversion by taking a church type and a substitution list
  to apply
*)
(*let rec alpha_conv e env : chexpression =
  match e with
  | Var(s) -> Var(subs_var s env)

  | Pair(e1, e2) -> Pair((alpha_conv e1 env), (alpha_conv e2 env))

  | Apply(e1, e2) -> Apply((alpha_conv e1 env), (alpha_conv e2 env))

  | _ -> failwith "TODO the rest"

  and subs_var s env =
    match assoc_if s env with
    | None      -> s
    | Some(sub) -> sub
;;*)


let p0 = Pair( Var("y"), Var("x") );;
let p = Pair(Const("1"), Pair( Var("z"), Var("x") ) );;
let che = Letin("w", Cross(Int, Cross(Int, Int)), Pair(Const("1"), Pair(Var("z"),Var("x"))), Var("w"));;
let env = ("x","y") :: ("z","w") :: [];;
(*let res = alpha_conv che env;;
pretty_print_e res; print_endline("");;*) (* alpha-conversion *)
print_endline("\nFree variables \n");;
List.map (print_endline) (free_variable che);; (* free_variable *)
print_endline("\nBound variables \n");;
List.map (print_endline) (bound_variable che);; (* bound_variable *)
