
let rec arithmetic_hell list = match list with 
  | [] -> 0
  | [x] -> 0
  | x :: y :: [] -> if (x=y) then 1 else 0
  | x :: y :: tail -> 
    arithmetic_hell((x+y) :: tail) + arithmetic_hell((x - y) :: tail);;
      