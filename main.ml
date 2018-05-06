
(*
  main.ml

  Main file - commands to test

  Author : Luxon JEAN-PIERRE
*)

module E = Expression
module T = Type
module W = Walgo


(* just to test *)
let eval expr : unit =
  let ty, _ = W.infer [] expr in
  print_endline ( T.to_string ty );;


(* tests *)

(* constant values *)
print_endline ("> 42");;
eval ( E.Const("42") );;

print_endline ("\n> true and false");;
eval ( E.Const("true") );;
eval ( E.Const("false") );;


print_endline ("\n> + ");;
eval ( E.Const("+") );;
print_endline ("\n> fst");;
eval ( E.Const("fst") );;
print_endline ("\n>  snd");;
eval ( E.Const("snd") );;
print_endline ("\n> ifthenelse");;
eval ( E.Const("ifthenelse") );;
print_endline ("\n> fix");;
eval ( E.Const("fix") );;

(* pairs *)
(*print_endline ("\n> pairs");;
let p1 = E.Pair( E.Const("42"), E.Const("42") );;
let p2 = E.Pair( E.Const("42"), E.Const("true"));;
print_endline ("\n> ( 42, 42 )");;
eval ( p1 );;
print_endline ("\n> ( 42, true )");;
eval ( p2 );;
print_endline ("\n> ( 64, ( 42, ( 42, true ) ) )");;
eval ( E.Pair( E.Const("64"), E.Pair( E.Const("42"), p2 ) ) );;
print_endline ("\n> ( ( false, true ) , ( 42, 42 ) ) )");;
eval ( E.Pair( E.Pair( E.Const("false"), E.Const("true") ), p1 ) );;*)

(* lambda *)
(*print_endline ("\n> λx. x + x");;
let splus = E.Lambda( "x", E.Apply( E.Const("+"), E.Pair( E.Var("x"), E.Var("x") ) ) );;
eval ( splus );;

print_endline ("\n> (λx. x + x) 42");;
eval ( E.Apply( E.Lambda( "x", splus ), E.Const("42") ) );;*)

(* identity *)
(*print_endline ("\n> (λx.x)");;
eval ( E.Lambda( "x", E.Var("x") ) );;

print_endline ("\n> (λx.x) 42");;
eval ( E.Apply( E.Lambda( "x", E.Var("x") ), E.Const("42") ) );;

let id_fun = E.Apply( E.Lambda( "x", E.Var("x") ),
                      E.Lambda( "y", E.Apply( E.Const("+"),
                                              E.Pair( E.Var("y"), E.Var("y") ) ) ) ) in

print_endline ("\n> (λx.x) (λy. y + y)");
eval ( id_fun );
print_endline ("\n> (λx.x) (λy. y + y) 42");
eval ( E.Apply( id_fun, E.Const("42") ) );;*)

(* addition *)
(*print_endline ("\n> λx.λy. x + y ");;
let fplus       = E.Apply( E.Const("+"), E.Pair( E.Var("x"), E.Var("y") ) ) in
let fsum        = E.Lambda( "x", ( E.Lambda( "y", fplus ) ) ) in
let partial_sum = E.Apply( fsum, E.Const("42") ) in
eval ( fsum );
print_endline ("\n> (λx.λy. x + y ) 42");
eval ( partial_sum );
print_endline ("\n> (λx.λy. x + y ) 42 1");
eval ( E.Apply( partial_sum , E.Const("1") ) );;

print_endline ("\n> let f = λx.x in f 2");
let lambdaf = E.Lambda( "x", E.Var("x") ) in
let apply_f = E.Apply( lambdaf, E.Const("2") ) in
eval ( E.Letin( "f", lambdaf, apply_f ) );;*)

(* ( x, y ) → x + y *)
(*print_endline ("\n> (λx. fst(x) + snd(x)) <42, 1>");
let p = E.Pair( E.Const("42"), E.Const("64") ) in
let fp = E.Apply( E.Const("fst"), E.Var("z") ) in
let sp = E.Apply( E.Const("snd"), E.Var("z") ) in
let fplus       = E.Apply( E.Const("+"), E.Pair( E.Var("x"), E.Var("y") ) ) in
let fsum        = E.Lambda( "x", ( E.Lambda( "y", fplus ) ) ) in
let fpair = E.Lambda( "z", E.Apply( E.Apply( fsum, fp ), sp ) ) in
eval ( E.Apply( fpair, p ) );;*)

(* if-then-else *)
(*let one = E.Const("1") in
let two = E.Const("2") in
let autoplus = E.Lambda( "x", E.Apply( E.Const("+"), E.Pair( E.Var("x"), E.Var("x") ) ) ) in
let apif = E.Apply( E.Const("ifthenelse") , E.Pair( E.Const("true") , E.Pair( one, two ) ) ) in

print_endline ("\n> if true then 1 else 2");
eval ( apif );
print_endline ("\n> (if true then 1 else 2) + 1");
eval ( E.Apply( autoplus, apif ) );;*)

(* factorial - it doesn't work!!! *)
(*let x       = E.Var("x") in
let zero    = E.Const("0") in
let one     = E.Const("1") in
let minus   = E.Apply( E.Const("-"), E.Pair( x, one ) ) in
let equalv  = E.Apply( E.Const("="), E.Pair( x, zero ) ) in
let rec recfact = E.Apply( fact, minus )
and multx   = E.Apply( E.Const("*"), E.Pair( x, recfact ) )
and ifx     = E.Apply( E.Const("ifthenelse") , E.Pair( equalv, E.Pair( one, multx ) ) )
and lambdax = E.Lambda( "x", ifx )
and fact    = E.Lambda( "fact", lambdax)
and fixfact = E.Apply( E.Const("fix"), fact ) in
eval ( E.Apply(fact, E.Const("3") ) );;*)
