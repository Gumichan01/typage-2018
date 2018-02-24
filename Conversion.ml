
(*
  Conversion.ml

  This file handles alpha-conversion

  Author : Luxon JEAN-PIERRE
*)

Import module ChurchType

(*
  A substitution is just a pair <old_name, new_name> that represents {x / y}
  *)
type substitution = string * string

type subList = substitution list

(*
  TODO
  It operates the alpha-conversion by taking a church type and a substitution list
  to apply

  @note The function fails if a variable capture happened during the alpha-conversion
*)
let rec alphaconv : ChurchType.chexpression -> subList -> ChurchType.chexpression =
  failwith "TODO alpha Conversion"
