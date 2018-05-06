module T = Type
type substitution = Sub of T.itype * T.itype
type unifier = Unifier of substitution list
type equation = Eq of T.itype * T.itype
type system = System of equation list
exception RecursiveType
exception TypeMismatch
exception OccursCheck
exception Conflict
val print_debug : equation list -> unit
val to_eql : system -> equation list
val from_eql : equation list -> system
val is_variable : T.itype -> bool
val vars : T.itype -> T.itype -> bool
val varsl : T.itype -> equation list -> bool
val apply_subs : substitution -> T.itype -> T.itype
val substitute : substitution -> equation -> equation
val substitute_all : substitution -> equation list -> equation list
val system_without_subs : substitution -> equation list -> equation list
val variables_on_left : equation list -> bool
val distinct : equation list -> bool
val is_resolved : equation list -> bool
val to_unifier : equation list -> unifier
val unify : system -> unifier
val compose_unifier : unifier -> unifier -> substitution list
