open Object;;
open Vectors;;

type world = obj list

let initWorld : world = []

let addObj ob (w : world): world = ob :: w

(*sufficient to remove one instance since it is possible for multiple objects to exist
 with the same configuration per say*)
let rec removeObj ob (w : world) : world = 
    match w with
    | [] -> []
    | x::xs -> if  x <> ob then (x :: removeObj ob xs)
    else xs;;

let gravity = {x = 0.; y = -10.; z = 0.}

let g = 1.;; (*place holder for gravitational constant*)

let rec totalGP world = 
    match world with
    | [] -> {x = 0.; y = 0.; z = 0.};
    | obj::rest -> 
            let gm = (g *. obj.mass /. 
            ((magnitude obj.position)*.(magnitude obj.position)*.(magnitude obj.position))) in
            vectorSum (scalarProduct gm obj.position) @@ totalGP rest;;

let rec initGravity world = 
    match world with 
    | [] -> []
    | obj::rest -> 
            let gm          = (g *. obj.mass /. 
            ((magnitude obj.position)*.(magnitude obj.position)*.(magnitude obj.position))) in
            let potential   = vectorDiff (totalGP world) (scalarProduct gm obj.position) in
            let force       = vectorSum obj.force @@ scalarProduct obj.mass gravity in
            let force       = vectorSum force (scalarProduct obj.mass potential) in
            {position = obj.position; 
            velocity = obj.velocity; 
            force = force; mass = obj.mass} :: initGravity rest;;



let rec step dt world = 
    match world with
    | [] -> []
    | obj::rest -> (*semi implicit euler integration*)
            let force       = vectorSum obj.force @@ scalarProduct obj.mass gravity in
            let velocity    = vectorSum obj.velocity @@
                                scalarProduct dt @@ 
                                scalarProduct (1. /. obj.mass) force in
            let position    = vectorSum obj.position @@
                                scalarProduct dt velocity in
            {position = position;
            velocity = velocity;
            force = force;
            mass = obj.mass}:: step dt rest;;

(*https://research.ncl.ac.uk/game/mastersdegree/gametechnologies/physicstutorials/*)
