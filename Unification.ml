

(* Schema de typeAlpha*)
(*type term = *)

type typeSchema =
   Alpha of string
  | Bool
  | Int
  | Arrow of typeSchema * typeSchema
  | Cross of typeSchema * typeSchema


let unify (tslist : (typeSchema * typeSchema) list) : unit =
  match tslist  with
  | [] -> print_string("done") (* todo return something? *)
  | h::q ->

and operate : (typeSchema * typeSchema) -> unit =
  function
  | (a,a) ->
  | _ -> expr2
