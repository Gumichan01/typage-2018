type itype =
    IBool
  | IInt
  | ICross of itype * itype
  | IArrow of itype * itype
  | IVar of string
val unify_all : (itype * itype) list -> unit
val unify : itype * itype
val delete : ('a * 'a) list -> ('a * 'a) list
val swap : (itype * itype) list -> (itype * itype) list
