
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

let rec free_variable = function
  | Tvar(_) as tv -> tv

  | Arrow( Int, Int )  | Arrow( Bool, Bool )
  | Arrow( Int, Bool ) | Arrow( Bool, Int )
  | Cross( Int, Int )  | Cross( Bool, Bool )
  | Cross( Int, Bool ) | Cross( Bool, Int ) as ty -> ty

  | Arrow( Int, y )  -> Arrow( Int, (free_variable y) )
  | Arrow( Bool, y ) -> Arrow( Bool, (free_variable y) )
  | Arrow( x, Int )  -> Arrow( (free_variable x), Int )
  | Arrow( x, Bool ) -> Arrow( (free_variable x), Bool )

  | Arrow( x, y ) -> Arrow( (free_variable x), (free_variable y) )

  | Cross( Int, y )  -> Cross( Int, (free_variable y) )
  | Cross( Bool, y ) -> Cross( Bool, (free_variable y) )
  | Cross( x, Int )  -> Cross( (free_variable x), Int )
  | Cross( x, Bool ) -> Cross( (free_variable x), Bool )

  | Cross( x, y ) -> Cross( (free_variable x), (free_variable y) )

  | _ -> assert false (* Int and Bool are not variables *)
