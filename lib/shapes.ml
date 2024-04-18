open Object;;

type shape = 
    Sphere of int*obj
    | Box of int*int*int*obj
    | Cylinder of int*int*obj
;;

type collisionEntity = shape*obj

let interface shape1 shape2 = 
    match (shape1, shape2) with
    | (Sphere (r1, obj1), Sphere (r2, obj2)) ->  let sum_of_radii = (r1 + r2) in sum_of_radii * sum_of_radii
    | _ -> 0 (*TODO: Implement the function properly*)

