(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Test_helpers

let () =
  Printexc.record_backtrace true ;
  run_test @@ test_suite "LIGO" [
    Integration_tests.main ;
    Transpiler_tests.main ;
    Typer_tests.main ;
    Coase_tests.main ;
    Vote_tests.main ;
    Multisig_tests.main ;
    Multisig_v2_tests.main ;
    Replaceable_id_tests.main ;
    Time_lock_tests.main ;
    Testing_example_pascaligo.main ;
    Testing_example_cameligo.main ;
    Testing_example_religo.main
  ] ;
  ()
