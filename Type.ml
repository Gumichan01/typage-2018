
(*
  Type.ml

  Type to infer

  Author : Luxon JEAN-PIERRE
*)


type itype =
  | Bool
  | Int
  | Cross of itype * itype
  | Arrow of itype * itype
  (* temporay type I am using *)
  | Tvar of string

let rec to_string = function
  | Int -> "int"
  | Bool -> "bool"
  | Cross(x, y) -> "(" ^(to_string x) ^ " × " ^ (to_string y) ^ ")"
  | Arrow(x, y) -> (to_string x) ^ " → " ^ (to_string y)
  | Tvar(s) -> s
