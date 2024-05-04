(*
  Tests for the lambda calculus parser and reducers.

  EXTEND THIS FILE TO TEST YOUR SOLUTION THOROUGHLY!
*)

open Utils
open Parser
open Reducer


let rec evaluate ~verbose reduce t =
  if verbose then print_string (format_term t) else ();
  match reduce t with
  | None -> 
    if verbose then print_string " =/=>\n\n" else ();
  | Some t' -> 
    if verbose then print_string " ==>\n\n" else ();
    evaluate ~verbose reduce t'


let s1 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
"

let s2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) tru)
"

let s3 = "
((\\id1.(t1 id1)) (\\id2.(t1 t2)))
"



let s4 = "
((\\id1.(t1 id1)) ((\\id2.(t1 id2)) a))
"

let s5 = "
((\\x.(\\y.(x y))) (\\z.(\\w.(z w))))
"


let s6="
(((\\x.(t x)) (\\x.(t x))) ((\\x.(t x)) (\\x.(t x))))
"

let s7 = "
((\\id1.(id1 t1)) ((\\id2.(id2 t1)) a))
"

let s8 = "
(\\x.x)
"

let s9 = "
(\\x.(\\y.x))
"

let s10 = "
((\\x.(\\y.(y x))) ((\\z.z) a))
"


let () =
  printf "\nEvaluating s1:\n%s\n" s1;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s1));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s1));

  printf "\nEvaluating s2:\n%s\n" s2;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s2));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s2));
  
  printf "\nEvaluating s3:\n%s\n" s3;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s3));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s3));

  printf "\nEvaluating s4:\n%s\n" s4;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s4));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s4));

  printf "\nEvaluating s5:\n%s\n" s5;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s5));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s5));

  printf "\nEvaluating s6:\n%s\n" s6;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s6));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s6));
  
  printf "\nEvaluating s7:\n%s\n" s7;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s7));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s7));
  
  printf "\nEvaluating s8:\n%s\n" s8;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s8));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s8));

  printf "\nEvaluating s9:\n%s\n" s9;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s9));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s9));
  
  printf "\nEvaluating s10:\n%s\n" s10;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s10));
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s10));
