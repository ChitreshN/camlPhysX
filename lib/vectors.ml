type vector = {
    x : float;
    y : float;
    z : float;
}


let magnitude v = sqrt (v.x *. v.x +. v.y *. v.y +. v.z *. v.z);;

let vectorSum v1 v2 = { x = v1.x +. v2.x ;
                        y = v1.y +. v2.y ;
                        z = v1.z +. v2.z};;

let vectorNeg v = { x = 0.0 -. v.x; y = 0.0 -. v.y; z = 0.0 -. v.z};;

let vectorDiff v1 v2 = vectorSum v1 @@ vectorNeg v2;;

let scalarProduct x v = { x = x *. v.x; y = x *. v.y; z = x *. v.z;};;

let dotProduct v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z;;

let crossProduct v1 v2 = {  x = v1.y *. v2.z -. v1.z *. v2.y;
                            y = v1.z *. v2.x -. v1.x *. v2.z;
                            z = v1.x *. v2.y -. v1.y *. v2.x};;

let vecCompProduct v1 v2 = {x = v1.x *. v2.x ;
                        y = v1.y *. v2.y ;
                        z = v1.z *. v2.z};;
