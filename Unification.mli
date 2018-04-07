module T = IType
type substitution = Sub of T.itype * T.itype | Bottom
type unifier = substitution list
type system = unifier
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val delete : ('a * 'a) list -> ('a * 'a) list
val is_resolved : system -> bool
val unify_aux : system -> unifier
val process : system -> system
val erase : 'a -> system
val replace : 'a -> 'b
val swap : 'a -> 'b
val decompose : system -> 'a
val unify : system -> unifier
