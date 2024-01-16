let list = [1; 2; 3];;

let rec compress arr = match arr with
      | [] -> []
      | x :: [] -> [x]
      | x :: y:: rest -> 
            if x = y then compress (y :: rest)
            else x :: (compress (y:: rest));;



let rec sum arr = match arr  with
      | [] -> 0
      | x :: rest -> x + sum rest;;


sum list;;

