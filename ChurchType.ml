
(*
  ChurchType.ml

  This file defines church type system

  Author : Luxon JEAN-PIERRE
*)

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

type environment = (string * chtype) list;;


let rec pretty_print_t =
  function
  | Bool -> print_string("Bool")
  | Int  -> print_string("Int")
  | Cross(x,y) -> print_string("Cross("); pretty_print_t x; print_string(", "); pretty_print_t y; print_string(")")
  | Arrow(x,y) -> print_string("Apply()"); pretty_print_t x; print_string(", "); pretty_print_t y; print_string(")")


let rec pretty_print_e =
  function
  | Const(s) -> print_string(String.concat "Const " [s])
  | Var(s)   -> print_string(String.concat "Var " [s])
  | Pair(a,b) -> print_string("Pair ("); pretty_print_e a;
                 print_string(", "); pretty_print_e b;print_string(")")
  | Apply(a,b) -> print_string("Apply "); pretty_print_e a;
                  print_string(" "); pretty_print_e b
  | Lambda(x,t,e) -> print_string("Lambda  "); pretty_print_e e;
                         print_string(": "); pretty_print_t t;

  | Letin(x,t,e1,e2) -> print_string("Let "); print_string x;
                        print_string(": t = "); pretty_print_e e1; print_string(" in "); pretty_print_e e2;;


let tc (env: environment) =
  function
  | Const s -> List.assoc s env
  | _       -> failwith "tc: invalid expression"


let rec type_check (env: environment) =
  function
  | Const s as cs -> tc env cs
  | Var(s)        -> List.assoc s env

  | Pair(a1, a2) ->
    let t1 = type_check env a1 in
    let t2 = type_check env a2 in Cross(t1, t2)

  | Apply(m, n) -> check_apply_type env m n

  | Lambda(x, t, e) ->
    (
      match t = (type_check env e) with
      | false -> failwith "LetIn type checking: invalid type"
      | true  -> t
    )
  | Letin(x, t, e1, e2) ->
    match t = (type_check env e1) with
    | false -> failwith "LetIn type checking: invalid type"
    | _ -> let nenv = (x, t)::env in (type_check nenv e2)

and check_apply_type (env: environment) m n =
  let ntype = type_check env n in
  match (type_check env m) with
  | Arrow(atype, rtype) when atype == ntype -> rtype
  | _ -> failwith "apply type checking: invalid type"
