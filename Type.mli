type itype =
    Bool
  | Int
  | Cross of itype * itype
  | Arrow of itype * itype
  | Tvar of string
val to_string : itype -> string
