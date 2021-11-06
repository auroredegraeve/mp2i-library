(*calculer la taille d'une liste*)
let rec taille l = match l with
  | [] -> 0  (* une liste vide est de taille 0 *)
  | e::q -> 1 + taille q;;  (* sinon l contient e + tous les éléments de q *)
  
  
(*calculer la somme des éléments d'une liste*)
let rec somme l = match l with
  | [] -> 0
  | e::q -> e + somme q;;


(*calculer la moyenne des éléments d'une liste*)
let moyenne l = (float_of_int (somme l)) /. (float_of_int (taille l));;
  
  
(*renvoyer le minimum*)  
let rec minimum l = match l with
    | [] -> max_int  (* par convention, le min d'un ensemble vide est +infini *)
                     (* c'est pratique pour que n'importe quelle valeur remplace +infini *)
    | e::q -> min e (minimum q);;  (* en utilisant la fonction min *)


(*renvoyer le maximum*)  
let rec maximum l = match l with
    | [] -> min_int  (* par convention, le max d'un ensemble vide est -infini *)
                     (* c'est pratique pour que n'importe quelle valeur remplace -infini *)
    | e::q -> max e (maximum q);;  (* en utilisant la fonction max *)
           
        
(*couper une liste en 2*)
let rec split l = match l with
    | [] -> [], []
    | [e] -> [e], []  (* si la liste n'a qu'un élément e *)
    | e1::e2::q -> let q1, q2 = split q in
                   e1::q1, e2::q2


(*concaténer 2 listes*)
let rec concat l1 l2 = match l1 with
    | [] -> l2
    | e::q -> e::concat q l2;;


(*inverser une liste*)
let rec rev acc l = match l with  (* acc va servir à construire le résultat (la liste à l'envers) *)
    | [] -> acc
    | e::q -> rev (e::acc) q;;


(*doublon*)
let rec doublon l = match l with
    | [] -> false
    | e::q -> List.mem e q || doublon q;;
