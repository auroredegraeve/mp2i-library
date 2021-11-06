(*tri fusion*)
let rec fusion l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | e1::q1, e2::q2 -> if e1 < e2 then e1::fusion q1 l2
                        else e2::fusion l1 q2;;


(* tri rapide *)
let rec partition l pivot = match l with
    | [] -> [], []
    | e::q -> let l1, l2 = partition q pivot in
              if e < pivot then e::l1, l2 (*l1 est une liste contenant les éléments de l inférieurs strictement à p*)
              else l1, e::l2;; (*l2 est une liste contenant les éléments de l supérieurs ou égaux à p*)
              
let rec quicksort = function
    | [] -> []
    | p::q -> let l1, l2 = partition q p in (*sépare la liste en 2 en fonction de p*)
              (quicksort l1) @ (p::quicksort l2);; (*concatène les 2 listes triées*)
