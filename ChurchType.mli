type chtype =
    Bool
  | Int
  | Cross of chtype * chtype
  | Arrow of chtype * chtype
type chexpression =
    Var of string
  | Const of string
  | Pair of chexpression * chexpression
  | Apply of chexpression * chexpression
  | Lambda of string * chtype * chexpression
  | Letin of string * chtype * chexpression * chexpression
type environment = (string * chtype) list
val pretty_print_t : chtype -> unit
val pretty_print_e : chexpression -> unit
val tc : environment -> chexpression -> chtype
val type_check : environment -> chexpression -> chtype
val check_apply_type : environment -> chexpression -> chexpression -> chtype
