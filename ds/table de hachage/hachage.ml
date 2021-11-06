(* création de la table de hachage *)
type ('k, 'v) hashtable = {
    t : ('k * 'v) option array;
    h : 'k -> int
};;


(* ajouter une clé et son élément *)
let hashtable_add ht (k, v) =
    ht.t.(ht.h k) <- Some v;;


(* obtenir l'élément d'une clé *)
let hashtable_get ht k =
    ht.t.(ht.h k);;


(*supprimer une clé *)
let hashtable_del ht k =
    ht.t.(ht.h k) <- None;;
