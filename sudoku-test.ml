#use "./sudoku.ml";;
#use "topfind";;
#require "OUnit2";;
open OUnit2

let suite =
  "TestSudokuEstConforme" >::: [
      "test_sudoku_tres_difficile_conforme" >:: fun _ ->
        assert (est_conforme sudoku_tres_difficile)
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
