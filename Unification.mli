module T = IType
type substitution = T.itype * T.itype
type unifier = substitution list
type system = unifier
val unify : system -> unifier
val delete : ('a * 'a) list -> ('a * 'a) list
val swap : (T.itype * T.itype) list -> (T.itype * T.itype) list
