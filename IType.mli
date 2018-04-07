type itype =
    IBool
  | IInt
  | ICross of itype * itype
  | IArrow of itype * itype
  | IVar of string
module TVar :
  sig
    type t = string
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val create : unit -> itype
  end
