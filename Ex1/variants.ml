type shape = 
      | Circle of float
      | Square of float
      | Rectangle of float * float;;


let area shape = match shape with
      | Circle radius -> radius *. radius *. 3.14159
      | Square edge -> edge *. edge
      | Rectangle (edge1,edge2) -> edge1 *. edge2;;


let rec total_area list = match list  with
      | [] -> 0.
      | x :: tail -> area (x) +. total_area(tail);;
