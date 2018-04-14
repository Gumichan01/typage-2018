module T = Type
type substitution = Sub of T.itype * T.itype
type unifier = substitution list
type equation = Eq of T.itype * T.itype
type system = equation list
exception RecursiveType
exception TypeMismatch
exception OccursCheck
exception Conflict
val is_variable : T.itype -> bool
val vars : T.itype -> T.itype -> bool
val varsl : T.itype -> equation list -> bool
val sub : substitution -> T.itype -> T.itype
val substitute : substitution -> equation -> equation
val substitute_all : substitution -> equation list -> equation list
val system_without_subs : substitution -> equation list -> equation list
val distinct : system -> bool
val is_resolved : system -> bool
val to_unifier : system -> unifier
val unify : system -> unifier
