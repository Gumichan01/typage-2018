module T = Type
type substitution = T.itype * T.itype
type unifier = substitution list
type system = unifier
exception RecursiveType
exception TypeMismatch
exception OccursCheck
exception Conflict
val delete : ('a * 'a) list -> ('a * 'a) list
val is_variable : T.itype -> bool
val vars : T.itype -> T.itype -> bool
val varsl : T.itype -> (T.itype * T.itype) list -> bool
val sub : substitution -> T.itype -> T.itype
val substitute : substitution -> T.itype * T.itype -> T.itype * T.itype
val substitute_all : substitution -> system -> system
val system_without_subs : substitution -> system -> system
val distinct : system -> bool
val is_resolved : system -> bool
val unify : system -> unifier
val to_string : T.itype -> string
val printI : T.itype * T.itype -> unit
