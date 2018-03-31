
(*
  Walgo.ml

  This file implements the W-algorithm

  Author : Luxon JEAN-PIERRE
*)

open ChurchType;;

(* Expression to infer the type of *)
type expression =
    Var of string
  | Const of string
  | Pair of expression * expression
  | Apply of expression * expression
  | Lambda of string * expression
  | Letin of string * expression * expression

(* Type to infer *)
type itype =
  | IBool
  | IInt
  | ICross of itype * itype
  | IArrow of itype * itype
  (* temporay type I am using *)
  | IVar of string

module V = struct
  type t = string
  let compare v1 v2 = Pervasives.compare v1 v2
  let equal v1 v2 = v1 = v2
  let create = let r = ref 0 in fun () -> incr r; IVar("α" ^ string_of_int(!r))
end

let math_ops = ["+"; "-"; "*"; "/"]
let bool_ops = ["&&"; "||"]
let math_basictype = IArrow(ICross(IInt, IInt), IInt)
let bool_basictype = IArrow(ICross(IBool, IBool), IBool)

(*
  There is a function that does the job in Ocaml 4.06, but I don't have this version.
  So I implemented it myself
*)
let lassoc_opt e l =
  try
      Some(List.assoc e l)
  with
  | _ -> None


(* Typing environment *)
type environment = (string * itype) list

(* substitution *)
type unifier = (itype * itype) list

let rec infer_program : expression list -> itype =
  failwith "TODO inference + W-algorithm"

let rec infer (delta : environment) (e : expression) =
  match e with
  | Var(_) | Const(_) as cv -> (inst delta cv, [])
  | Pair(n, l) ->
    let b, rhob = infer delta n in
    let c, rhoc = infer (sigma delta rhob) l in
    (ICross(b, c), []) (* change it *)

  | Apply(_,_) -> failwith "TODO W-algorithm: Apply"

  | Lambda(x, n) -> (*failwith "TODO W-algorithm: Lambda"*)
    let fresh_alpha =  (V.create ()) in
    let b, rho = infer ((x, fresh_alpha)::delta) n in
    (IArrow(fresh_alpha, b), []) (* change it *)

  | Letin(_,_,_) -> failwith "TODO W-algorithm: Letin"

  (*
    I want to make the following calculation

    Let Γ = x₁ : A₁, ..., xₙ : Aₙ an environment, and σ a type substitution.
    So σ(Γ) = x₁ : σ(A₁), ..., xₙ : σ(Aₙ)

  *)
  and sigma (delta: environment) (sub: unifier) =
    match sub with
    | [] -> delta
    | _ -> match delta with [] -> delta | _ ->  sigma_in delta sub

  (* pre-condition: sub is not an empty list *)
  and sigma_in (delta: environment) (sub: unifier) =
    match delta with
    | [] -> delta
    | (x, a)::q ->
      (match lassoc_opt a sub with
       | Some(t) -> (x, t) :: sigma_in q sub
       | None -> (x, a) :: sigma_in q sub
      )

  (* Get the type instance of the variable or the constant value *)
  and inst env = function
  | Var(s)   -> List.assoc s env
  | Const(x) -> inst_constv x
  | _-> assert(false) (* type instance *)

  (* Get the type instance of constant value *)
  and inst_constv x =
    (match inst_intv x with
     | Some(_) -> IInt
     | None ->
       (match x with
        | "true" | "false" -> IBool
        | _ -> assert(false) (* pre-condoition: integer or boolean value *)
       )
    )
  and inst_intv s =
    try
      Some(int_of_string s)
    with
    | _ -> None



(*
    Comment:

    - (TODO final goal) Apply the algorithm for each element of type chtype (an expression).
      (TODO) replace a type by another using the substitution
      (TODO) σ₁ o σ₂ function
    - (TODO) Unification
    - (DONE) Free and bound variables
    - (TODO) α-conversion

*)
