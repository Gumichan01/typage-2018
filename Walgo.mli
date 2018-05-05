module T = Type
module E = Expression
module U = Unification
type expression = E.t
val lassoc_opt : 'a -> ('a * 'b) list -> 'b option
val from_sblist : U.substitution list -> (U.T.itype * U.T.itype) list
type environment = (string * T.itype) list
type unifier = U.unifier
val cunifier :
  U.substitution list -> U.substitution list -> U.substitution list
val substype : U.substitution list -> U.T.itype -> U.T.itype
val gen : T.itype -> 'a -> T.itype
val infer : environment -> expression -> T.itype * U.substitution list
val sigma : environment -> U.substitution list -> environment
val sigma_in : environment -> U.substitution list -> environment
val inst : environment -> E.t -> T.itype
val inst_constv : environment -> string -> T.itype
val inst_intv : string -> int option
val eval : expression -> unit
