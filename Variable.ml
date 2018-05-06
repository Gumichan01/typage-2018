
(*
  Variable.ml

  @note I thought I will need this file, but seems that I don't need it

  Author : Luxon JEAN-PIERRE
*)

module E = Expression

(* miscelleanous function *)
let assoc_if a l =
  try
      Some( List.assoc a l )
  with
  | _ -> None

(*
  Generate the list of free variables
*)
let free_variable chexpr =
  let rec aux_fv bvl = function
    | E.Var(s) ->
      begin
        match ( List.mem s bvl ) with
        | true  -> []
        | false -> [ s ]
      end
    | E.Const( _ ) -> []
    | E.Pair( m, n )        -> ( aux_fv bvl m ) @ ( aux_fv bvl n )
    | E.Apply( m, n )       -> ( aux_fv bvl m ) @ ( aux_fv bvl n )
    | E.Lambda( x, _, m )   -> ( aux_fv ( x :: bvl ) m )
    | E.Letin( x, _, m, n ) -> ( aux_fv ( x :: bvl) m ) @ ( aux_fv ( x :: bvl ) n )

  in aux_fv [] chexpr


(*
  Generate the list of bound variables
*)
let rec bound_variable = function
  | E.Var( _ ) | E.Const( _ ) -> []
  | E.Pair( m, n ) | E.Apply( m, n ) -> (bound_variable m) @ (bound_variable n)
  | E.Lambda( x, _, m )   -> (bound_variable m) @ [x]
  | E.Letin( x, _, m, n ) -> (bound_variable m) @ (bound_variable n) @ [x]
