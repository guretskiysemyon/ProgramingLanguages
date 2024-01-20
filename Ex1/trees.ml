type 'a binary_tree = 
      | Empty
      | Node of 'a * 'a binary_tree * 'a binary_tree;;


let rec less list piv = match list with
        |[] -> []
        | x :: rest -> 
             if x <= piv then x :: (less rest piv)
             else (less rest piv);;


let rec more list piv = match list with
        |[] -> []
        | x :: rest -> 
            if x > piv then x :: (more rest piv)
            else (more rest piv);;


let rec construct list = match list with
        | [] ->  Empty
        | x :: rest -> Node(x, construct(less rest x), construct(more rest x));;



