type expression =
    Var of string
  | Const of string
  | Pair of expression * expression
  | Apply of expression * expression
  | Lambda of string * expression
  | Letin of string * expression * expression
module T = IType
module V :
  sig
    type t = string
    val compare : 'a -> 'a -> int
    val equal : 'a -> 'a -> bool
    val create : unit -> T.itype
  end
val math_ops : string list
val bool_ops : string list
val math_basictype : T.itype
val bool_basictype : T.itype
val lassoc_opt : 'a -> ('a * 'b) list -> 'b option
type environment = (string * T.itype) list
type unifier = (T.itype * T.itype) list
val infer_program : expression list -> T.itype
val infer : environment -> expression -> T.itype * unifier
val sigma : environment -> unifier -> environment
val sigma_in : environment -> unifier -> environment
val inst : environment -> expression -> T.itype
val inst_constv : string -> T.itype
val inst_intv : string -> int option
