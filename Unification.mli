module T = IType
type substitution = T.itype * T.itype
type unifier = substitution list
type system = unifier
exception RecursiveType
exception TypeMismatch
exception OccursCheck
exception Conflict
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val delete : ('a * 'a) list -> ('a * 'a) list
val is_resolved : system -> bool
val unify_aux : system -> unifier
val process : system -> system
val erase : system -> system
val replace : system -> system
val swap : system -> system
val decompose : system -> system
val check : T.itype * T.itype -> T.itype * T.itype
val occurs_check : T.itype * T.itype -> bool
val conflict : T.itype * T.itype -> bool
val unify : system -> unifier
val to_string : T.itype -> string
val printI : T.itype * T.itype -> unit
