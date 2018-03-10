type substitution = string * string
type subList = substitution list
val assoc_if : 'a -> ('a * 'b) list -> 'b option
val free_variable : ChurchType.chexpression -> string list
val bound_variable : ChurchType.chexpression -> string list
val alpha_conv :
  ChurchType.chexpression ->
  (string * string) list -> ChurchType.chexpression
val subs_var : string -> (string * string) list -> string
val p0 : ChurchType.chexpression
val p : ChurchType.chexpression
val che : ChurchType.chexpression
val env : (string * string) list
