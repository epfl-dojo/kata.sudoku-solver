#use "./sudoku.ml";;
#use "topfind";;
#require "OUnit2";;
open OUnit2

let suite =
  "Test Suite" >::: [
      "est_conforme" >::: [
        ( "sudoku_tres_difficile est conforme" >::
            fun _ -> assert (est_conforme sudoku_tres_difficile)
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
