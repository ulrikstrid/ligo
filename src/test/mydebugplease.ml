Printexc.record_backtrace true ;
Printf.printf "Start of mydebugplease executable\n%!";
let _ = Typer_tests.TestExpressions.lambda () in
Printf.printf "End of mydebugplease executable\n%!"
