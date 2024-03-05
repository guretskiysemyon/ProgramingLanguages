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

let s1_result_cbn = "
(\\t.(\\f.t))
"

let s1_result_cbv = "
(\\t.(\\f.t))
"

let s2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) tru)
"

let s2_result_cbn = "
(\\t.(\\f.f))
"

let s2_result_cbv = "
(\\t.(\\f.f))
"

let s3 = "
((\\id1.(t1 id1)) (\\id2.(t1 t2)))
"

let s3_result_cbn = "
(t1 (\\id2.(t1 t2)))
"

let s3_result_cbv = "
(t1 (\\id2.(t1 t2)))
"

let s4 = "
((\\id1.(t1 id1)) ((\\id2.(t1 id2)) a))
"

let s4_result_cbn = "
(t1 ((\\id2.(t1 id2)) a))
"

let s4_result_cbv = "
((\\id1.(t1 id1)) (t1 a))
"

let s5 = "
((\\x.(\\y.(x y))) (\\z.(\\w.(z w))))
"

let s5_result_cbn = "
(\\y.((\\z.(\\w.(z w))) y))
"

let s5_result_cbv = "
(\\y.((\\z.(\\w.(z w))) y))
"

let s6 = "
((\\x.(\\y.(x y))) ((\\z.((\\w.(z w)) (\\v.v))) ((\\u.u) a)))
"

let s6_result_cbv = "
((\\x.(\\y.(x y))) (a (\\v.v)))
"

let s6_result_cbn = "
(\\y.(((\\z.((\\w.(z w)) (\\v.v))) ((\\u.u) a)) y))
"

let s7 = "
((\\id1.(id1 t1)) ((\\id2.(id2 t1)) a))
"

let s7_result_cbv = "
((\\id1.(id1 t1)) (a t1))
"

let s7_result_cbn = "
((a t1) t1)
"

let s8 = "
(\\x.x)
"

let s8_result_cbv = "
(\\x.x)
"

let s8_result_cbn = "
(\\x.x)
"

let s9 = "
(\\x.(\\y.x))
"

let s9_result_cbn = "
(\\x.(\\y.x))
"

let s9_result_cbv = "
(\\x.(\\y.x))
"

let s10 = "
((\\x.(\\y.(y x))) ((\\z.z) a))
"

let s10_result_cbv = "
(\\y.(y a))
"

let s10_result_cbn = "
(\\y.(y ((\\z.z) a)))
"

let s11 = "
((((\\x.(\\y.(\\z.(x (y z))))) (\\a.a)) b) c)
"

let s11_result_cbn = "
(b c)
"

let s11_result_cbv = "
((\\a.a) (b c))
"

let s12 = "
((\\x.(\\y.x)) (\\z.(\\w.(\\v.(\\u.(z (w (v u))))))))
"

let s12_result_cbv = "
(\\y.(\\z.(\\w.(\\v.(\\u.(z (w (v u))))))))
"

let s12_result_cbn = "
(\\y.(\\z.(\\w.(\\v.(\\u.(z (w (v u))))))))
"

let () =
  printf "\nEvaluating s1:\n%s\n" s1;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s1));
  printf "Designated result of s1_cbn: %s\n" s1_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s1));
  printf "Designated result of s1_cbv: %s\n\n" s1_result_cbv;

  printf "\nEvaluating s2:\n%s\n" s2;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s2));
  printf "Designated result of s2_cbn: %s\n" s2_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s2));
  printf "Designated result of s2_cbv: %s\n\n" s2_result_cbv;
  
  printf "\nEvaluating s3:\n%s\n" s3;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s3));
  printf "Designated result of s3_cbn: %s\n" s3_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s3));
  printf "Designated result of s3_cbv: %s\n\n" s3_result_cbv;

  printf "\nEvaluating s4:\n%s\n" s4;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s4));
  printf "Designated result of s4_cbn: %s\n" s4_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s4));
  printf "Designated result of s4_cbv: %s\n\n" s4_result_cbv;

  printf "\nEvaluating s5:\n%s\n" s5;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s5));
  printf "Designated result of s5_cbn: %s\n" s5_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s5));
  printf "Designated result of s5_cbv: %s\n\n" s5_result_cbv;

  printf "\nEvaluating s6:\n%s\n" s6;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s6));
  printf "Designated result of s6_cbn: %s\n" s6_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s6));
  printf "Designated result of s6_cbv: %s\n\n" s6_result_cbv;
  
  printf "\nEvaluating s7:\n%s\n" s7;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s7));
  printf "Designated result of s7_cbn: %s\n" s7_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s7));
  printf "Designated result of s7_cbv: %s\n\n" s7_result_cbv;
  
  printf "\nEvaluating s8:\n%s\n" s8;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s8));
  printf "Designated result of s8_cbn: %s\n" s8_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s8));
  printf "Designated result of s8_cbv: %s\n\n" s8_result_cbv;
  
  printf "\nEvaluating s9:\n%s\n" s9;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s9));
  printf "Designated result of s9_cbn: %s\n" s9_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s9));
  printf "Designated result of s9_cbv: %s\n\n" s9_result_cbv;
  
  printf "\nEvaluating s10:\n%s\n" s10;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s10));
  printf "Designated result of s10_cbn: %s\n" s10_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s10));
  printf "Designated result of s10_cbv: %s\n\n" s10_result_cbv;

  printf "\nEvaluating s11:\n%s\n" s11;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s11));
  printf "Designated result of s11_cbn: %s\n" s11_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s11));
  printf "Designated result of s11_cbv: %s\n\n" s11_result_cbv;
  
  printf "\nEvaluating s12:\n%s\n" s12;
  printf "in CBN semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbn (parse s12));
  printf "Designated result of s12_cbn: %s\n" s12_result_cbn;
  printf "in CBV semantics:\n\n";
  ignore (evaluate ~verbose:true reduce_cbv (parse s12));
  printf "Designated result of s12_cbv: %s\n\n" s12_result_cbv;

