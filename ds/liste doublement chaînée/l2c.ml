type 'a l2c = {elem : 'a; mutable prev : 'a l2c; mutable next : 'a l2c};;


(* renvoyer une l2c contenant seulement e *)
let create e =
    let rec l = {elem = e; prev = l; next = l} in
    l;;


(* ajouter un élément après l *)
let add l e =
    let l_new = {elem = e; prev = l; next = l.next} in
    l.next.prev <- l_new;
    l.next <- l_new;;


(* supprimer l *)
let del l =
    l.prev.next <- l.next;
    l.next.prev <- l.prev;;


(* calculer la taille d'une l2c *)
let length l =
    let rec aux l1 =
        if l1 == l then 1 else 1 + aux l1.next in
    aux l.next;;


(* chercher un élément e dans l *)
let mem e l =
    let cur = ref l.next in
    while !cur.elem <> e && !cur != l do
        cur := !cur.next
    done;
    !cur.elem = e;;


(* fusionner 2 l2c *)
let fusion l1 l2 =
    (l1.next).prev <- l2;
    (l2.next).prev <- l1;
    let l1n = l1.next in
    l1.next <- l2.next;
    l2.next <- l1n;;
