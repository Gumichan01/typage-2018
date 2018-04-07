module T = IType
type substitution = T.itype * T.itype
type unifier = substitution list
type system = unifier
exception RecursiveType
exception TypeMismatch
exception OccursCheck
exception Conflict
val delete : ('a * 'a) list -> ('a * 'a) list
val is_resolved : system -> bool
val unify_aux : system -> unifier
val process : system -> system
val erase : substitution list -> substitution list
val replace : substitution list -> substitution list
val swap : substitution list -> substitution list
val decompose : system -> substitution list
val check : substitution -> substitution
val occurs_check : substitution -> bool
val conflict : substitution -> bool
val unify : system -> unifier
val to_string : T.itype -> string
val printI : T.itype * T.itype -> unit
