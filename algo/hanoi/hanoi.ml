(*tour de hanoi*)
let rec hanoi n tige1 tige2 =
  if n = 0 then () (* aucun déplacement à faire *)
  else (let tige_intermediaire = 3 - tige1 - tige2 in (* on utilise le fait que la somme des 3 tiges vaut 3 *)
        hanoi (n - 1) tige1 tige_intermediaire;
        print_string ((string_of_int tige1)^" -> "^(string_of_int tige2)^"\n");
        hanoi (n - 1) tige_intermediaire tige2);;
