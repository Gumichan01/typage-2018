
(*
  IType.ml

  Type to infer

  Author : Luxon JEAN-PIERRE
*)


type itype =
  | IBool
  | IInt
  | ICross of itype * itype
  | IArrow of itype * itype
  (* temporay type I am using *)
  | IVar of string

module TVar = struct
  type t = string
  let compare v1 v2 = Pervasives.compare v1 v2
  let equal v1 v2 = v1 = v2
  let create = let r = ref 0 in fun () -> incr r; IVar("α" ^ string_of_int(!r))
end
