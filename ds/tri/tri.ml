(*tri fusion*)
let rec fusion l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | e1::q1, e2::q2 -> if e1 < e2 then e1::fusion q1 l2
                        else e2::fusion l1 q2;;
