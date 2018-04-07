module T = IType
type substitution = Sub of T.itype * T.itype | Bottom
type unifier = substitution list
type system = unifier
val delete : ('a * 'a) list -> ('a * 'a) list
val swap : (T.itype * T.itype) list -> (T.itype * T.itype) list
val unify_aux : system -> unifier
val unify : system -> unifier
