type shape = 
      | Circle of float
      | Square of float
      | Rectangle of float * float;;


let area shape = match shape with
      | Circle radius -> radius *. radius *. 3.14159
      | Square edge -> edge *. edge
      | Rectangle (edge1,edge2) -> edge1 *. edge2;;
