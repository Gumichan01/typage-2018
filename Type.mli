type itype =
    Bool
  | Int
  | Cross of itype * itype
  | Arrow of itype * itype
  | Tvar of string
val to_string : itype -> string
module V : sig type t = string val create : unit -> itype end
val gen_type : itype -> itype
