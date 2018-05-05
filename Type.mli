type itype =
    Bool
  | Int
  | Cross of itype * itype
  | Arrow of itype * itype
  | Tvar of string
val to_string : itype -> string
module V :
  sig
    type t = string
    val compare : 'a -> 'a -> int
    val equal : 'a -> 'a -> bool
    val create : unit -> itype
  end
