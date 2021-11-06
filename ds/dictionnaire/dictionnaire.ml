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
    }
};;
