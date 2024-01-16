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





let rec insert tree n = match tree with
        | Empty -> Node(n, Empty, Empty)
        | Node (v, l, r) ->
                if n <= v then
                        Node(v, insert l n, r)
                else
                        Node(v, l, insert r n);;

let create_tree list = create_tree_list Empty list;;

let rec create_tree_list tree list = match list with
        | [] -> tree
        | x :: rest -> create_tree_list (insert tree x) rest;;
