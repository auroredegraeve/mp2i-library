type 'a queue_2lists = {l1 : 'a list; l2 : 'a list};;


(* vérifier si la file est vide *)
let queue_empty f = 
    f.l1 = [] && f.l2 = [];;


(* ajouter un élément *)
let queue_add f e = 
    {l1 = e::f.l1; l2 = f.l2};;


(*supprimer un élément *)
(* suppose que f est non vide *)
(* renvoie (élément, liste obtenue) *)
let rec queue_pop f = match f.l1 with
    | e::q -> e, {l1 = q; l2 = f.l2}
    | [] -> queue_pop {l1 = List.rev f.l2; l2 = []};;
