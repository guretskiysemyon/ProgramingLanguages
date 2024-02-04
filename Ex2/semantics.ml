let default_state x =  0;;



let natural_numbers_semantic n = n 0;;

let rec aritmetic_semantic e s = match e with
    | Num n ->  natural_numbers_semantic n
    | Var v -> s v
    | Add (a_1, a_2) -> (aritmetic_semantic a_1 s) + (aritmetic_semantic a_2 s)
    | Mult (a_1, a_2) -> (aritmetic_semantic a_1 s) * (aritmetic_semantic a_2 s)
    | Sub (a_1, a_2) -> (aritmetic_semantic a_1 s) - (aritmetic_semantic a_2 s);;




let rec boolean_semantic e s = match e with
  | TT -> true
  | FF -> false
  | Aeq (a_1, a_2) -> (aritmetic_semantic a_1 s) = (aritmetic_semantic a_2 s)
  | Beq  (b_1, b_2) -> (boolean_semantic b_1 s) = (boolean_semantic b_2 s)
  | Leq (a_1, a_2) -> (aritmetic_semantic a_1 s) <= (aritmetic_semantic a_2 s)
  | Neg b -> not (boolean_semantic b s)
  | And (b_1,b_2) -> (boolean_semantic b_1 s) && (boolean_semantic b_2 s);;







let create_state prev_state x e = fun (y) -> 
  if x = y then 
    aritmetic_semantic e prev_state 
  
  else prev_state y;;




let rec nos c = match c with
  | (Ass (v,e),s) -> create_state s v e
  | (Skip, s) -> s
  | (Comp (c_1, c_2),s) -> nos(c_2, nos(c_1, s))
  | (If (b, c_1, c_2),s ) -> if (boolean_semantic b s) then nos (c_1, s) else nos(c_2, s)
  | (While (b, c),s) -> if (boolean_semantic b s) then
                                  nos (Comp(c, While (b,c)), s)
                        else nos (Skip, s)

  | (If_Ass (v,e,c_1,c_2),s) -> nos (Comp(
                            Ass(v,e),
                            If( Aeq(e, Num (fun _ -> 0)), c_2, c_1)),s)
  | (Repeat (c, b),s) -> nos (
                            Comp(c,
                              If( Beq(b, TT),Skip, Repeat(c,b))
                            ),s
                          );;


                        

                          