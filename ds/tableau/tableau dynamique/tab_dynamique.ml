(* création du tableau *)
type 'a array_dyn = {mutable t : 'a array; mutable n : int};;

(* ajouter un élément dans le tableau *)
let copy t1 t2 = (* copie t1 dans t2 *)
    for i = 0 to Array.length t1 - 1 do
        t2.(i) <- t1.(i)
    done;;

let add e d =
    if d.n < Array.length d.t then (d.t.(d.n) <- e; d.n <- d.n + 1)
    else if d.n = 0 then (d.t <- [|e|]; d.n <- 1)
    else (let t' = Array.make (2*d.n) d.t.(0) in
        copy d.t t';
        t'.(d.n) <- e;
        d.t <- t';
        d.n <- d.n + 1);;
