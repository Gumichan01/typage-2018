
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

module V = struct
  type t = string
  let compare v1 v2 = Pervasives.compare v1 v2
  let equal v1 v2 = v1 = v2
  let create = let r = ref 0 in fun () -> incr r; Tvar("α" ^ string_of_int(!r))
end

(*let free_variable = function*)
