
(*
  Unification.ml

  This file defines the unification

  Author : Luxon JEAN-PIERRE
*)

module T = Type


(*
  technically → (Tvar, T.itype {Tvar / real type})
*)
(*type substitution = T.itype * T.itype*)
type substitution = Sub of T.itype * T.itype

(* The most greatest unifier *)
type unifier = Unifier of substitution list

(* System to unify *)
type equation = Eq of T.itype * T.itype
type system   = System of equation list

exception RecursiveType
exception TypeMismatch
exception OccursCheck
exception Conflict


(* debug *)
let print_debug l =
  let printE ( Eq( a, b ) ) =
    print_endline ( ( T.to_string a ) ^ " = " ^ ( T.to_string b ) )
  in
    begin
        print_endline( "========" );
        ignore ( List.map printE l );
        print_endline ( "========" )
    end

(*
    to_eql: equation list -> system
    from_eql: system -> equation list
*)
let to_eql ( System( eql ) ) = eql
let from_eql eql = System( eql )

let is_variable = function
  | T.Tvar( _ ) -> true
  | _ -> false

(*
    Checks if a variable belongs to the variables of a term
*)
let rec vars alpha = function
  | T.Tvar( _ ) as a when alpha = a -> true
  | T.Tvar( _ ) | T.Int | T.Bool    -> false
  | T.Cross( m, n ) -> ( vars alpha m ) || ( vars alpha n )
  | T.Arrow( a, b ) -> ( vars alpha a ) || ( vars alpha b )

(*
    Checks if a variable is in a system (equation list)
*)
let rec varsl alpha = function
  | []   -> false
  | ( Eq( a, b ) ) :: q ->
    begin
      match ( vars alpha a ), ( vars alpha b ) with
      | true, _
      | _ , true  -> true
      | _ -> varsl alpha q
    end

(* Recursively replace a 'variable' (α) with the associated term *)
let apply_subs ( Sub( alpha, term ) ) expr =
  let rec sub_aux alpha' term' = function
    | x when x = alpha' -> term'
    | T.Cross( m, n )  ->
      T.Cross( ( sub_aux alpha' term' m ), ( sub_aux alpha' term' n ) )

    | T.Arrow( m, n )  ->
      T.Arrow( ( sub_aux alpha term' m ), ( sub_aux alpha term' n ) )

    | _ as i -> i

  in sub_aux alpha term expr


let substitute s ( Eq( a, t ) ) = Eq( (apply_subs s a), (apply_subs s t) )

let substitute_all s sys = List.map ( substitute s ) sys

let rec system_without_subs ( Sub(a, b) ) = function
  | [] -> []
  | Eq( m, n ) :: q when m = a && n = b -> q
  | h :: q ->
    begin
        let s = Sub( a, b ) in
        h :: ( system_without_subs s q )
    end

(*
    For each pair ( a, b ), check if a is a variable.
    return true if every equations have a variable on the left, false otherwise
*)
let rec variables_on_left = function
  | [] -> true
  | ( Eq( a, _ ) ) :: q -> ( is_variable a ) && ( variables_on_left q )


(*
    pre-condition:  { α₀ = t₀, ..., αₙ₋₁ = tₙ₋₁ }

    Every α are variables. t could be anything
*)
let distinct l =
  let rec d_aux l hashtbl =
    match l with
    | [] -> true
    | ( Eq( T.Tvar(a), _ ) ) :: q ->
      begin
        if Hashtbl.mem hashtbl a then
          false
        else
          begin
            Hashtbl.add hashtbl a 0;
            d_aux q hashtbl
          end
      end
    | ( Eq( a, b ) )::q ->
      begin
          print_endline ( "! " ^ ( T.to_string a ) );
          print_endline ( "! " ^ ( T.to_string b ) );
          assert false (* pre-condition *)
      end
  in d_aux l ( Hashtbl.create (List.length l) )

let is_resolved =
  ( fun l -> ( variables_on_left l ) && ( distinct l ) )


let to_unifier sys =
  let rec aux_u l acc =
    match l with
    | [] -> Unifier( acc )
    | ( Eq( m, n ) )::q  -> aux_u q ( ( Sub( m, n ) ) :: acc )
  in aux_u sys []


(** The function that makes the unification *)

let unify sys : unifier =
  let rec unify_aux sys' =
    begin
      let nsys = process sys' in
      if is_resolved nsys then
        to_unifier nsys
      else
        unify_aux nsys
    end

  and process l = List.map check ( l |> decompose |> swap |> eliminate |> erase )

  (*and process_debug l =
    let dl = decompose l in
    let sl = swap dl in
    let el = eliminate sl in
    let eel = erase el in
    print_endline ( "equation system input" ); print_debug l;
    print_endline ( "equation system decomp" ); print_debug dl;
    print_endline ( "equation system swap" ); print_debug sl;
    print_endline ( "equation system eliminate " ); print_debug el;
    print_endline ( "equation system erase" ); print_debug eel;
    List.map check eel*)

  and erase l =
    let rec aux_erase sl res =
      match sl with
      | [] -> res
      | ( Eq( s1, s2 ) ) :: q when s1 = s2 -> aux_erase q ( res )
      | h :: q -> aux_erase q ( h :: res )
    in aux_erase l []

  and eliminate l = (*eliminate_aux l l*)
    let rec eliminate_aux g acc =
      match g with
      | [] -> acc
      | ( Eq( a, t ) ) :: q when (is_variable a) ->
        begin
          let eq = Eq( a, t ) in
          let s  = Sub( a, t ) in
          let ng = system_without_subs s g in (* E \ { a ← t } *)
          if not( vars a t ) && ( varsl a ng ) then
            begin
              eliminate_aux ( eq :: ( substitute_all s ng ) ) acc (* E' U { a = t } *)
            end
          else
            eliminate_aux q ( ( Eq( a, t ) ) :: acc )
        end
      | h :: q -> eliminate_aux q ( h :: acc )

    in eliminate_aux l []

  and swap l =
    let rec aux_swap sl res =
      match sl with
      | [] -> res
      | ( Eq( T.Tvar(s), x ) ) :: q
      | ( Eq( x, T.Tvar(s) ) ) :: q -> aux_swap q ( ( Eq( T.Tvar(s), x ) ) :: res )
      | h :: q -> aux_swap q ( h :: res )
    in aux_swap l []

  and decompose sys =
    let rec decompose_aux sys' acc =
      match sys' with
      | [] -> acc
      | ( Eq( T.Cross( u, w ), T.Cross( v, x ) ) ) :: q
      | ( Eq( T.Arrow( u, w ), T.Arrow( v, x ) ) ) :: q ->
        decompose_aux q ( ( Eq( u, v ) ) :: ( Eq( w, x ) ) :: acc )

      | h::q -> decompose_aux q ( h :: acc )

    in decompose_aux sys []

  (*
      Check if a substitution is not ill-formed
      -> occurs check and conflict
  *)
  and check p =
    if occurs_check p then
      raise OccursCheck
    else
      if conflict p then
        raise Conflict
      else p

  (*
      . α = α × α  - recursive type
      . α = α -> α - recursive type
  *)
  and occurs_check = function
    | Eq( T.Tvar( s ), T.Cross( x, y ) )
    | Eq( T.Tvar( s ), T.Arrow( x, y ) ) ->
      begin
          let a = T.Tvar( s ) in
          vars a x && vars a y
      end
    | _ -> false

  (*
      . f(s₀, ..., sₖ) = g(t₀, ..., tₙ)     f != g or k != n
      . int = bool
      . bool = int
      . α × α = α -> α
  *)
  and conflict = function
    | Eq( T.Int, T.Bool ) | Eq( T.Bool, T.Int )
    | Eq( T.Int, T.Cross( _ ) )  | Eq( T.Cross( _ ), T.Int )
    | Eq( T.Int, T.Arrow( _ ) )  | Eq( T.Arrow( _ ), T.Int )
    | Eq( T.Bool, T.Cross( _ ) ) | Eq( T.Cross( _ ), T.Bool )
    | Eq( T.Bool, T.Arrow( _ ) ) | Eq( T.Arrow( _ ), T.Bool )
    | Eq( T.Cross( _ ), T.Arrow( _ ) ) | Eq( T.Arrow( _ ), T.Cross( _ ) ) -> true
    | _ -> false

  in unify_aux ( to_eql sys )


(*
  Unifier - composition
*)

(* compose_unifier g f ≡ f ∘ g *)
let compose_unifier g f =
  (* [ e = v ] { v / v' } -> [e = v'] *)
  let comp_sub s ( Sub( e, v ) ) = Sub( e, ( apply_subs s v ) ) in
  (* for each substitution v, apply it to every elements in g *)
  let comp_map g v = List.map ( comp_sub v ) g in
  let rec comp_aux g' = function
    | [] -> g'
    | s :: q -> comp_aux ( s :: ( comp_map g' s ) ) q
  in
    begin
        let ( Unifier( g' ) ) = g in
        let ( Unifier( f' ) ) = f in
        comp_aux g' f'    (* f ∘ g *)
    end
