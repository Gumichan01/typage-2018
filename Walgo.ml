
(*
  Walgo.ml

  This file implements the W-algorithm

  Author : Luxon JEAN-PIERRE
*)

module T = Type
module E = Expression
module U = Unification

type expression = E.t

(* Typing environment - Δ *)
type environment = ( string * T.itype ) list

(* unifier - μ *)
type unifier = U.unifier


(*
  There is a function that does the job in Ocaml 4.06, but I don't have this version.
  So I implemented it myself
*)
let lassoc_opt e l =
  try
      Some( List.assoc e l )
  with
  | _ -> None


let from_sblist sblist =
  List.map (fun ( U.Sub( a, b ) ) -> ( a, b ) ) sblist

(* unifier composition - f ∘ g *)
let cunifier g f = U.compose_unifier ( U.Unifier( g ) ) ( U.Unifier( f ) )

(*
    Substitute a type by replacing it with another type, if it can be replaced
    returns the argument itself otherwise
 *)
let substype rho a =
  match ( lassoc_opt a ( from_sblist rho ) ) with
  | Some(t) -> t
  | None -> a

(*
    Generalization of a type

    Gen(A, Γ) = ∀( α1 , ..., αn ).A |{ α1 , ..., αn } = VarLib(A) \ VarLib(Γ)
*)
let gen ty delta = ignore(delta); T.gen_type ty


(*
    W-algorithm
*)
let rec infer (delta : environment) (e : expression) =
  match e with
  (* W((∆, x : A), x) = (inst(A), id )  *)
  | E.Var( _ )

  (* W(∆, cte) = (inst(A), id), A is the type of the constant value cte *)
  | E.Const( _ ) as cv -> ( inst delta cv, [] )

  (* W (∆, < N, L >) = (ρC(B) × C, ρC ◦ ρB),
     W(∆, N) = (B, ρB),
     W(ρB(∆), L) = (C, ρC) *)
  | E.Pair( n, l ) ->
    let b, rhob = infer delta n in
    let c, rhoc = infer ( sigma delta rhob ) l in
    ( T.Cross( ( substype rhoc b ), c ), ( cunifier rhob rhoc ) )

  (* W(∆, N L) = (μ(α), μ ◦ ρC ◦ ρB ),
     with W(∆, N) = (B, ρB), W (ρB(∆), L) = (C, ρC), α is a fresh variable and
     μ = MGU (ρc(B ) = C → α) *)
  | E.Apply( n, l ) ->
    let b, rhob = infer delta n in
    let c, rhoc = infer ( sigma delta rhob ) l in
    let alpha = ( T.V.create () ) in
    let eql = [ U.Eq( ( substype rhoc b ), T.Arrow( c, alpha ) ) ] in
    let ( U.Unifier( mgu ) ) = U.unify ( U.from_eql eql ) in
    ( ( substype mgu alpha ), cunifier ( cunifier rhob rhoc ) mgu )

  (* W (∆, λx.N) = (ρ(α) → B, ρ),
     with W((∆, x : α), N) = (B, ρ), α is a fresh variable *)
  | E.Lambda( x, n ) ->
    let fresh_alpha =  ( T.V.create () ) in
    let b, rho = infer ( ( x, fresh_alpha )::delta ) n in
    ( T.Arrow( ( substype rho fresh_alpha ), b ), rho )

  (* W(∆, let x = N in L) = (C, ρC ◦ ρB ), où W (∆, N) = (B, ρB),
     W((ρB(∆), x : Gen(B, ρB(∆))), L) = (C, ρC) *)
  | E.Letin( x, n, l ) ->
    let b, rhob = infer delta n in
    let sigdelta = sigma delta rhob in
    let xtype = gen b sigdelta in
    let c, rhoc = infer ( ( x, xtype ) :: sigdelta ) l in
    ( c, ( cunifier rhob rhoc ) )

  (*
    I want to make the following calculation

    Let Γ = x₁ : A₁, ..., xₙ : Aₙ an environment, and σ a type substitution.
    So σ(Γ) = x₁ : σ(A₁), ..., xₙ : σ(Aₙ)
  *)
  and sigma ( delta: environment ) sub =
    match sub with
    | [] -> delta
    | _  ->
      begin
          match delta with
          | [] -> delta
          | _ ->  sigma_in delta sub
      end

  (* pre-condition: sub is not an empty list *)
  and sigma_in ( delta: environment ) sub =
    match delta with
    | [] -> delta
    | ( x, a ) :: q ->
      begin
        match ( lassoc_opt a ( from_sblist sub ) ) with
        | Some(t) -> (x, t) :: sigma_in q sub
        | None -> (x, a) :: sigma_in q sub
      end

  (*
    Get the type instance of the variable or the constant value

    TC( + | - | * | / ): (int × int) → int
    TC( = | != | < | > | <= | >= ): (int × int) → bool
    TC( and | or | xor ): (bool × bool) → bool
    TC( not ): bool → bool
    TC( fst ): (α × β) → α
    TC( snd ): (α × β) → β
    TC( ifthenelse ): ( bool × α × α ) → α
    TC( fix ): ( α → α ) → α
  *)
  and inst env = function
  | E.Var( s )   -> List.assoc s env
  | E.Const( x ) -> inst_constv env x
  | _-> assert( false ) (* type instance *)


  (* Get the type instance of constant value *)
  and inst_constv env x =
    match inst_intv x with
     | Some( _ ) -> T.Int
     | None ->
       begin
           match x with
           | "true" | "false" -> T.Bool
           | "+" | "-" | "*" | "/" -> T.Arrow( T.Cross( T.Int, T.Int ), T.Int )
           | "=" | "!=" | "<" | ">" | "<=" | ">=" -> T.Arrow( T.Cross( T.Int, T.Int ), T.Bool )
           | "and" | "or" | "xor" -> T.Arrow( T.Cross( T.Bool, T.Bool ), T.Bool )
           | "not" -> T.Arrow( T.Bool, T.Bool )
           | "fst" ->
             begin
               let a1 = ( T.V.create () ) in
               let a2 = ( T.V.create () ) in
               T.Arrow( T.Cross( a1, a2 ), a1 )
             end

           | "snd" ->
             begin
               let a1 = ( T.V.create () ) in
               let a2 = ( T.V.create () ) in
               T.Arrow( T.Cross( a1, a2 ), a2 )
             end

           | "ifthenelse" ->
              begin
                let a = ( T.V.create () ) in
                T.Arrow( T.Cross( T.Bool, T.Cross( a, a ) ), a )
              end

           | "fix" ->
             begin
               let a = ( T.V.create () ) in
               T.Arrow( T.Arrow( a, a ), a )
             end

           | _ -> assert(false) (* pre-condition: integer or boolean value *)
       end

  and inst_intv s =
    try
      Some( int_of_string s )
    with
    | _ -> None
