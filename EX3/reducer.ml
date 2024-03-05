(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))


let fresh_var used_vars : string = 
  if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
  then raise (OutOfVariablesError)
  else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)
(* 

(* AST for lambda expressions - DO NOT MODIFY THIS TYPE *)
type term = | Variable of string
	    | Abstraction of string * term
	    | Application of term * term *)


let rec fv = fun t -> match t with
  | Variable (s) -> StringSet.singleton s
  | Abstraction (s, term) -> StringSet.remove s (fv term)
  | Application (term1, term2) -> StringSet.union (fv term1) (fv term2);;

(*
  ADD FUNCTIONS BELOW
*) 


let rec substitute x s = fun t -> match t with
  | Variable (id) -> if x = id then s else Variable(id)
  | Abstraction(id, term) when (id != x && not(StringSet.mem id (fv s))) -> Abstraction(id, substitute x s term)
  | Abstraction (id, term ) when (id != x && (StringSet.mem id (fv s ))) -> 
              let fresh_v = fresh_var(StringSet.union (fv s) (StringSet.union (fv term) (StringSet.singleton x))) in 
                            (substitute x s (Abstraction (fresh_v, (substitute id (Variable fresh_v) term))))
  | Abstraction (id,term) -> Abstraction(id,term)
  | Application(t1, t2) -> Application((substitute x s t1), (substitute x s t2))
 



let is_abstraction t = match t with 
    | Abstraction (_,_) -> true
    | _ -> false


let is_variable t = match t with 
| Variable (_) -> true
| _ -> false


let rec reduce_cbv = fun t -> match t with
| Application (t1, t2) when not (is_abstraction t1) -> 
    let red_t1 = reduce_cbv t1 in
    (match red_t1 with 
    | Some e -> Some (Application(e, t2))
    | None ->
        let red_t2 = reduce_cbv t2 in
        (match red_t2 with
        | Some e -> Some (Application(t1, e))
        | None -> None
        )
    )
| Application(Abstraction(id, t1), t2) when (is_variable t2) -> Some(substitute id t2 t1)
| Application (t1, t2) when not (is_abstraction t2) ->
        let red_t2 = reduce_cbv t2 in
        (match red_t2 with
        | Some e -> Some (Application(t1, e))
        | None -> None
        )
| Application (Abstraction(id1, t1), t2) -> Some (substitute id1 t2 t1)
| _ -> None
  

  
 

let rec reduce_cbn = fun t -> match t with
| Application (t1, t2) when not (is_abstraction t1) -> 
  let red_t1 = (reduce_cbn t1) in (
            match red_t1 with 
            | Some (e) -> Some(Application(e, t2))
            | None -> None
  )
| Application(Abstraction(id, t1), t2) when (is_variable t2) -> Some(substitute id t2 t1)
| Application (Abstraction(id1, t1),t2) -> Some(substitute id1 t2 t1)
| _ -> None
