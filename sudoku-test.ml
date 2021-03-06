#use "./sudoku.ml";;
#use "topfind";;
#require "OUnit2";;
open OUnit2

let sudoku_faux_en_lignes : sudoku = [
    (*        v------ Ici *)
    [ Nb 9 ; Nb 9 ; Vide ;     Vide ; Vide ; Vide ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Vide ; Vide ; Nb 1 ;     Vide ; Vide ; Nb 7 ];
    [ Nb 5 ; Vide ; Vide ;     Vide ; Vide ; Nb 3 ;     Vide ; Vide ; Nb 4 ];

    [ Vide ; Vide ; Nb 7 ;     Vide ; Vide ; Vide ;     Nb 2 ; Vide ; Vide ];
    [ Vide ; Vide ; Nb 3 ;     Nb 6 ; Vide ; Nb 8 ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Nb 4 ; Vide ; Nb 3 ;     Nb 6 ; Nb 1 ; Vide ];

    [ Vide ; Nb 8 ; Nb 5 ;     Vide ; Nb 4 ; Vide ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Nb 3 ; Nb 2 ; Vide ;     Vide ; Nb 6 ; Vide ];
    [ Vide ; Nb 4 ; Vide ;     Vide ; Nb 1 ; Vide ;     Vide ; Nb 9 ; Vide ]
  ]


let sudoku_faux_en_colonnes : sudoku = [
    [ Nb 9 ; Vide ; Vide ;     Vide ; Vide ; Vide ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Vide ; Vide ; Nb 1 ;     Vide ; Vide ; Nb 7 ];
    [ Nb 5 ; Vide ; Vide ;     Vide ; Vide ; Nb 3 ;     Vide ; Vide ; Nb 4 ];

    [ Vide ; Vide ; Nb 7 ;     Vide ; Vide ; Vide ;     Nb 2 ; Vide ; Vide ];
    [ Vide ; Vide ; Nb 3 ;     Nb 6 ; Vide ; Nb 8 ;     Vide ; Vide ; Vide ];
    (*                            Ici ----------v *)
    [ Vide ; Vide ; Vide ;     Nb 4 ; Vide ; Nb 3 ;     Nb 6 ; Nb 1 ; Vide ];

    [ Vide ; Nb 8 ; Nb 5 ;     Vide ; Nb 4 ; Vide ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Nb 3 ; Nb 2 ; Vide ;     Vide ; Nb 6 ; Vide ];
    [ Vide ; Nb 4 ; Vide ;     Vide ; Nb 1 ; Vide ;     Vide ; Nb 9 ; Vide ]
  ]


let sudoku_faux_en_petits_carres : sudoku = [
    [ Nb 9 ; Vide ; Vide ;     Vide ; Vide ; Vide ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Vide ; Vide ; Nb 1 ;     Vide ; Vide ; Nb 7 ];
    [ Nb 5 ; Vide ; Vide ;     Vide ; Vide ; Nb 3 ;     Vide ; Vide ; Nb 4 ];

    [ Vide ; Vide ; Nb 7 ;     Vide ; Vide ; Vide ;     Nb 2 ; Vide ; Vide ];
    [ Vide ; Vide ; Nb 3 ;     Nb 6 ; Vide ; Nb 8 ;     Vide ; Vide ; Vide ];
    (*   v---- Ici *)
    [ Nb 7 ; Vide ; Vide ;     Nb 4 ; Vide ; Vide ;     Nb 6 ; Nb 1 ; Vide ];

    [ Vide ; Nb 8 ; Nb 5 ;     Vide ; Nb 4 ; Vide ;     Vide ; Vide ; Vide ];
    [ Vide ; Vide ; Vide ;     Nb 3 ; Nb 2 ; Vide ;     Vide ; Nb 6 ; Vide ];
    [ Vide ; Nb 4 ; Vide ;     Vide ; Nb 1 ; Vide ;     Vide ; Nb 9 ; Vide ]
  ]


let suite =
  "Test Suite" >::: [
      "place" >::: [
         "par l'exemple" >::
           fun _ -> assert_equal (place sudoku_faux_en_colonnes 6 3) (Nb 2)
      ] ;
      "petit_carre" >::: [
        ( "par l'exemple" >::
            fun _ -> assert_equal (petit_carre sudoku_faux_en_colonnes 6 3)
                       [Nb 2; Vide; Vide; Vide; Vide; Vide; Nb 6; Nb 1; Vide]
        ) ;
      ] ;
      "est_conforme" >::: [
        ( "sudoku_tres_difficile est conforme" >::
            fun _ -> assert (est_conforme sudoku_tres_difficile)
        ) ;
        ( "sudoku_faux_en_lignes n'est pas conforme" >::
           fun _ -> assert (not (est_conforme sudoku_faux_en_lignes))
        ) ;
        ( "sudoku_faux_en_colonnes n'est pas conforme" >::
            fun _ -> assert (not (est_conforme sudoku_faux_en_colonnes))
        ) ;
        ( "sudoku_faux_en_petits_carres n'est pas conforme" >::
            fun _ -> assert (not (est_conforme sudoku_faux_en_petits_carres))
        ) ;

        ( "sudoku mal carré → pas conforme" >::
            fun _ -> let malcarre = [ [ Vide ; Vide ; Vide ];
                                       [ Vide ; Vide ; Vide ];
                                       [ Vide ; Vide ; Vide ]] in
                    try
                      assert_equal false (not (est_conforme malcarre))
                    with Failure _ -> ()
        ) ;
      ];
      "racine_carree_entiere" >::: [
          (
            "9 → 3" >::
              fun _ -> assert_equal 3 (racine_carree_entiere 9)
          ) ;
          ( "8 → erreur" >::
              fun _ -> try
                      begin
                        let _ = racine_carree_entiere 8 in () ;
                        assert_failure "racine_carree_entiere 8 devrait échouer"
                      end
            with Failure _ -> ()
          ) ;
        ];
      "est_bien_carre" >::: [
          "est_bien_carre : oui" >:: fun _ -> assert (est_bien_carre sudoku_tres_difficile)
        ];
      "pas_de_doublons" >::: [
          ("sans doublons" >::
             fun _ -> assert_equal true (pas_de_doublons [Vide; Vide; Nb 1; Nb 3])
          );
          ("avec doublons" >::
             fun _ -> assert_equal false (pas_de_doublons [Vide; Vide; Nb 1; Nb 3; Nb 1])
          );
        ];
      "remplace" >::: [
          ("par l'exemple" >::
             fun _ -> assert_equal ["a"; "b"; "c"; "f"; "e"] (
                          remplace ["a"; "b"; "c"; "d"; "e"] 3 "f")
          ) ;
        ];
      "remplace2" >::: [
          ("par l'exemple" >::
             fun _ -> assert_equal [[ Nb 1; Vide; Vide; Vide ];
                                    [ Nb 2; Vide; Vide; Vide ];
                                    [ Nb 3; Vide; Vide; Vide ];
                                    [ Vide; Nb 4; Vide; Vide ]] (
                          remplace2 [[ Nb 1; Vide; Vide; Vide ];
                                     [ Nb 2; Vide; Vide; Vide ];
                                     [ Nb 3; Vide; Vide; Vide ];
                                     [ Vide; Vide; Vide; Vide ]] 1 3 (Nb 4))
          ) ;
        ];
    ]

(************************************************************************)
(* Il y a quelque chose de ce genre dans OUnit2, mais il n'est
   pas exporté :( *)
let run_test_tt suite =
  let  conf =  OUnitConf.default () in
  let _ =
  OUnitCore.run_test_tt conf
    (OUnitLoggerStd.std_logger conf OUnitLogger.shard_default)
    (snd (OUnitRunner.choice conf))
    (snd (OUnitChooser.choice conf))
    suite in
  ()

let () = run_test_tt suite
