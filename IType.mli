type itype =
    IBool
  | IInt
  | ICross of itype * itype
  | IArrow of itype * itype
  | IVar of string
module TVar :
  sig
    type t = string
    val compare : 'a -> 'a -> int
    val equal : 'a -> 'a -> bool
    val create : unit -> itype
  end
