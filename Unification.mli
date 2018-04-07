module T = IType
val unify_all : (T.itype * T.itype) list -> unit
val unify : T.itype * T.itype
val delete : ('a * 'a) list -> ('a * 'a) list
val swap : (T.itype * T.itype) list -> (T.itype * T.itype) list
