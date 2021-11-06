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


type ('k, 'v) dict = {
    add : 'k * 'v -> unit;
    del : 'k -> unit;
    get : 'k -> 'v option
};;


let dict_of_hashtable n =
    let ht = {
        t = Array.make n None;
        h = fun k -> k mod n
        } in {
            add = hashtable_add ht;
            get = hashtable_get ht;
            del = hashtable_del ht
    };;
