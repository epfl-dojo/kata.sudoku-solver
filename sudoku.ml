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

let est_conforme sudoku =
  let plage = 0 --^ (List.length sudoku)
  in
  let colonne i = List.map (fun j -> place sudoku i j) plage
  in
  let colonnes = List.map colonne plage
  in
  est_bien_carre sudoku
  &&
    List.for_all pas_de_doublons sudoku
  &&
    List.for_all pas_de_doublons colonnes

