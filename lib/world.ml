open Object;;

type world = obj list

let initWorld : world = []

let addObj ob (w : world): world = ob :: w

let rec removeObj ob (w : world) : world = 
    match w with
    | [] -> []
    | x::xs -> if  x <> ob then (x :: removeObj ob xs)
    else removeObj ob xs;;

(*
let step dt w = 
    match w with
    | [] -> []
    | x::xs -> 
 *)

let gravity = {x = 0.; y = -10.; z = 0.}

let rec step dt world = 
    match world with
    | [] -> []
    | obj::rest -> 
            let force       = vectorSum obj.force @@ scalarProduct obj.mass gravity in
            let velocity    = vectorSum obj.velocity @@
                                scalarProduct dt @@ 
                                scalarProduct (1. /. obj.mass) force in
            let position    = vectorSum obj.position @@
                                scalarProduct dt velocity in
            {position = position;
            velocity = velocity;
            force = force;
            mass = obj.mass}:: step dt rest
