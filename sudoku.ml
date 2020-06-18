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

let est_conforme sudoku = est_bien_carre sudoku
