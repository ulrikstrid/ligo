(* open Alcotest *)

let wrap f () =
  match f () with
  | Ok _ -> ()
  | Error err ->
    Format.printf "%a\n%!" (Ligo.Display.error_pp ~dev:true) (err ()) ;
    raise Alcotest.Test_error
 
let run case = Alcotest.run "isolated test" [ ("" , [ case ]) ]