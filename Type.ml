
(*
  Type.ml

  Type system + inference

  Author : Luxon JEAN-PIERRE
*)


type itype =
  | Bool
  | Int
  | Cross of itype * itype
  | Arrow of itype * itype
  (* temporay type I am using to infer the type *)
  | Tvar of string


let rec to_string = function
  | Int -> "int"
  | Bool -> "bool"
  | Cross( x, y ) -> "(" ^( to_string x ) ^ " × " ^ ( to_string y ) ^ ")"
  | Arrow( x, y ) -> ( to_string x ) ^ " → " ^ ( to_string y )
  | Tvar( s ) -> s

(*
    α type creation
*)
module V = struct
  type t = string
  let create =
    begin
      let r = ref 0 in
      fun () -> incr r; Tvar( "α" ^ string_of_int( !r ) )
    end
end

(*
  Generalization of a type

  Gen(int) = Gen(bool) = α
  Gen(int -> int) = Gen(bool -> bool) = α -> α
  Gen(int -> bool) = Gen(bool -> int) = α -> β

  Gen(int -> A) = Gen(bool -> A) = α -> Gen(A)
  Gen(int × A) = Gen(bool × A) = α × Gen(A)

  Gen(A -> int) = Gen(A -> bool) = Gen(A) -> β
  Gen(A × int) = Gen(A × bool) = Gen(A) × β

  Gen(A -> B) = Gen(A) -> Gen(B)
  Gen(A × B) = Gen(A) × Gen(B)
*)
let rec gen_type = function
  | Tvar( _ ) as tv -> tv

  | Bool | Int -> ( V.create () )

  | Arrow( Int, Int )  | Arrow( Bool, Bool ) ->
    let a = ( V.create () ) in Arrow( a, a )

  | Arrow( Int, Bool ) | Arrow( Bool, Int ) ->
    let a = ( V.create () ) in
    let b = ( V.create () ) in
    Arrow( a, b )

  | Arrow( Int, y )  -> Arrow( Int, ( gen_type y ) )
  | Arrow( Bool, y ) -> Arrow( Bool, ( gen_type y ) )
  | Arrow( x, Int )  -> Arrow( ( gen_type x ), Int )
  | Arrow( x, Bool ) -> Arrow( ( gen_type x ), Bool )

  | Arrow( x, y ) -> Arrow( ( gen_type x ), ( gen_type y ) )

  | Cross( Int, Int )  | Cross( Bool, Bool ) ->
    let a = ( V.create () ) in Cross( a, a )

  | Cross( Int, Bool ) | Cross( Bool, Int ) ->
    let a = ( V.create () ) in
    let b = ( V.create () ) in
    Cross( a, b )

  | Cross( Int, y )  -> Cross( Int, ( gen_type y ) )
  | Cross( Bool, y ) -> Cross( Bool, ( gen_type y ) )
  | Cross( x, Int )  -> Cross( ( gen_type x ), Int )
  | Cross( x, Bool ) -> Cross( ( gen_type x ), Bool )

  | Cross( x, y ) -> Cross( ( gen_type x ), ( gen_type y ) )
