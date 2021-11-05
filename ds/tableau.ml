(* échanger les références de 2 éléments d'un tableau *)
let swap t i j =
    let tmp = t.(i) in
    t.(i) <- t.(j);
    t.(j) <- tmp;
    t;;


(* calculer la somme des éléments d'un tableau *)
let somme t = 
    let res = ref 0 in
    for i=0 to Array.length t - 1 do
        res := !res + t.(i)
    done;
    !res;;


(* rechercher le minimum dans un tableau *)
let minimum t = 
    let m = ref t.(0) in
    for i=1 to Array.length t - 1 do
        m := min t.(i) !m
    done;
    !m;;


(* rechercher le maximum dans un tableau *)
let maximum t = 
    let m = ref t.(0) in
    for i=1 to Array.length t - 1 do
        m := max t.(i) !m
    done;
    !m;;


(* transformer un tableau en liste *)
let list_of_array t = 
    let rec aux i = (* transforme t.(0), ..., t.(i) en liste *)
        if i = -1 then []
        else t.(i)::aux (i - 1) in
    aux (Array.length t - 1);;
    

(* tester si un tableau est trié par ordre croissant *)
let croissant t =
    let res = ref true in
    for i=0 to Array.length t - 2 do
        if t.(i) > t.(i + 1)
        then res := false
    done;
    !res;;


(* calculer la tranche maximum d'un tableau *)
let tranche_max t =
    let m = ref t.(0) in
    let m_cur = ref t.(0) in
    for i = 1 to Array.length t - 1 do
        m_cur := max (!m_cur + t.(i)) t.(i);
        m := max !m !m_cur
    done;
    !m;;


(* rechercher par trichotomie *)
let tricho t e =
    let rec aux i j =
        if i > j then false
        else let m1 = (2*i + j + 1)/3 in
            let m2 = (i + 2*j + 2)/3 in
            if t.(m1) = e || t.(m2) = e then true
            else if e < t.(m1) then aux i (m1 - 1)
            else if e < t.(m2) then aux (m1 + 1) (m2 - 1)
            else aux (m2 + 1) j in
    aux 0 (Array.length t - 1);;
