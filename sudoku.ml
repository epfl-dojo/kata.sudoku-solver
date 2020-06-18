type case = Vide | Nb of int
type sudoku = case list list

let sudoku_tres_difficile : sudoku = [
    [ Nb 9 ; Vide ; Vide ;     Vide ; Vide ; Vide ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Vide ; Vide ; Nb 1 ;     Vide ; Vide ; Nb 7 ];
    [ Nb 5 ; Vide ; Vide ;     Vide ; Vide ; Nb 3 ;     Vide ; Vide ; Nb 4 ];

    [ Vide ; Vide ; Nb 7 ;     Vide ; Vide ; Vide ;     Nb 2 ; Vide ; Vide ];
    [ Vide ; Vide ; Nb 3 ;     Nb 6 ; Vide ; Nb 8 ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Nb 4 ; Vide ; Vide ;     Nb 6 ; Nb 1 ; Vide ];

    [ Vide ; Nb 8 ; Nb 5 ;     Vide ; Nb 4 ; Vide ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Nb 3 ; Nb 2 ; Vide ;     Vide ; Nb 6 ; Vide ];
    [ Vide ; Nb 4 ; Vide ;     Vide ; Nb 1 ; Vide ;     Vide ; Nb 9 ; Vide ]
  ];;

let racine_carree_entiere n =
  let r = sqrt (float_of_int n) in
  if r = floor r then
    int_of_float r
  else
    raise (Failure "n'est pas un carré")

let ordre sudoku = racine_carree_entiere (List.length sudoku)

let est_bien_carre sudoku =
  let _ = ordre sudoku
  and l = List.length sudoku
  in List.for_all (fun ligne -> List.length ligne == l) sudoku

let pas_de_doublons l =
  let pas_vides = List.filter (fun c -> c != Vide) l
  in
  List.length (List.sort_uniq compare pas_vides) = List.length pas_vides

let rec (--^) i j = if i >= j then [] else (i :: (--^) (i + 1) j)

let place sudoku i j = List.nth (List.nth sudoku j) i

let listMap2 f plage =
  let petite_ligne j = List.map (fun i -> f i j) plage
  in List.concat (List.map petite_ligne plage)

let petit_carre sudoku i0 j0 =
  listMap2 (fun i j -> place sudoku (i  + i0) (j + j0)) (0 --^ (ordre sudoku))

let petits_carres sudoku =
  let ordre = ordre sudoku
  in listMap2 (fun i j -> petit_carre sudoku (i * ordre) (j * ordre)) (0 --^ ordre)

let est_conforme_rapide_ sudoku =
  let plage = 0 --^ (List.length sudoku)
  in
  let colonne i = List.map (fun j -> place sudoku i j) plage
  in
  let colonnes = List.map colonne plage
  in
    List.for_all pas_de_doublons sudoku
  &&
    List.for_all pas_de_doublons colonnes
  &&
    List.for_all pas_de_doublons (petits_carres sudoku)

let est_conforme sudoku =
  est_bien_carre sudoku
  &&
  est_conforme_rapide_ sudoku


(***********************************************************)

let rec remplace liste offset v =
  match liste with
  | [] -> []
  | a :: reste -> match offset with
                  | 0 -> v :: reste
                  | n -> a :: remplace reste (n - 1) v

let remplace2 l i j v =
  remplace l j (remplace (List.nth l j) i v)

let cases_vides sudoku =
  let taille = List.length sudoku
  in
  List.filter
    (fun (i,j) -> Vide = place sudoku i j)
    (listMap2 (fun i j -> i, j) (0 --^ taille))

let chiffres_permis sudoku i j =
  let chiffres = 1 --^ (List.length sudoku + 1)
  in
  List.filter
    (fun n -> (est_conforme_rapide_ (remplace2 sudoku i j (Nb n)))
    ) chiffres

let min compare l =
  let rec min_ meilleur l = match l with
    | []      -> meilleur
    | a :: l' -> min_ (if (compare a meilleur) < 0 then a else meilleur) l'
  in match l with
  | []      -> raise (Failure "min de liste vide")
  | a :: l' -> min_ a l'

let rec resoudre sudoku : sudoku list =
  let cases_vides = cases_vides sudoku
  in
  match cases_vides with
  | [] -> if est_conforme sudoku then [sudoku] else []
  | _  ->
     let options : ((int * int) * int list) list =
       List.map (fun (i, j) -> ((i,j), chiffres_permis sudoku i j)) cases_vides
     in
     let meilleure_option = min (fun (_, chiffres_permis_a)
                                     (_, chiffres_permis_b) ->
                                (List.length chiffres_permis_a) - (List.length chiffres_permis_b)
                              ) options
     in
     let ((i, j), chiffres) = meilleure_option
     in List.concat (List.map (fun c -> resoudre (remplace2 sudoku i j (Nb c))) chiffres);;

let print_sudoku sudoku =
  let case_to_string c = match c with
    | Vide -> " "
    | Nb x -> Printf.sprintf "%d" x
  in
  let print_line l = print_endline (String.concat " " (List.map case_to_string l))
  in
  begin
    List.iter print_line sudoku ;
    print_newline ()
  end;;

List.iter print_sudoku (resoudre sudoku_tres_difficile)
