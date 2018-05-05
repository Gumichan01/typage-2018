
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


let rec gen_type = function
  | Tvar(_) as tv -> tv

  | Bool | Int -> ( V.create () )

  | Arrow( Int, Int )  | Arrow( Bool, Bool ) ->
    let a = ( V.create () ) in Arrow( a, a )

  | Arrow( Int, Bool ) | Arrow( Bool, Int ) ->
    let a = ( V.create () ) in
    let b = ( V.create () ) in
    Arrow( a, b )

  | Arrow( Int, y )  -> Arrow( Int, (gen_type y) )
  | Arrow( Bool, y ) -> Arrow( Bool, (gen_type y) )
  | Arrow( x, Int )  -> Arrow( (gen_type x), Int )
  | Arrow( x, Bool ) -> Arrow( (gen_type x), Bool )

  | Arrow( x, y ) -> Arrow( (gen_type x), (gen_type y) )

  | Cross( Int, Int )  | Cross( Bool, Bool ) ->
    let a = ( V.create () ) in Cross( a, a )

  | Cross( Int, Bool ) | Cross( Bool, Int ) as ty ->
    let a = ( V.create () ) in
    let b = ( V.create () ) in
    Cross( a, b )

  | Cross( Int, y )  -> Cross( Int, (gen_type y) )
  | Cross( Bool, y ) -> Cross( Bool, (gen_type y) )
  | Cross( x, Int )  -> Cross( (gen_type x), Int )
  | Cross( x, Bool ) -> Cross( (gen_type x), Bool )

  | Cross( x, y ) -> Cross( (gen_type x), (gen_type y) )
