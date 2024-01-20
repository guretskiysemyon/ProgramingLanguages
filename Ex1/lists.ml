
let rec compress arr = match arr with
      | [] -> []
      | x :: [] -> [x]
      | x :: y:: rest -> 
            if x = y then compress (y :: rest)
            else x :: (compress (y:: rest));;



let rec sum_list arr = match arr  with
      | [] -> 0
      | x :: rest -> x + sum_list rest;;




