open Unit_tests.Helpers
open Trace
open Michelson

let dummy = Mini_c.e_skip
let ignore_in = Mini_c.e_unit
let ignore_t = Mini_c.T_base (Base_unit)
let ignore_out = seq []

let if_none_env () =
  let mock_translate exp env =
    match exp,env with
    | e,[] when e=dummy -> simple_fail "should not be called back with an empty env"
    | _ -> ok ignore_out in

  let%bind (_:michelson) = Compiler.exp_if_none
      mock_translate
      (ignore_in, ignore_in, ((Var.of_name "new",ignore_t), dummy))
      [] in
  ok ()

let toto : unit Alcotest.test_case = ("if_none adds variable to env", `Quick, wrap if_none_env)

let () = run toto