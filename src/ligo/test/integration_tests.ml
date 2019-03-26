open Ligo_helpers.Trace
open Ligo
open Test_helpers

let pass (source:string) : unit result =
  let%bind raw =
    trace (simple_error "parsing") @@
    parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    simplify raw in
  let%bind typed =
    trace (simple_error "typing") @@
    type_ simplified in
  let%bind _mini_c =
    trace (simple_error "transpiling") @@
    transpile typed in
  ok ()

let basic () : unit result =
  pass "./contracts/toto.ligo"

let function_ () : unit result =
  let%bind _ = pass "./contracts/function.ligo" in
  let%bind _ = easy_run_main "./contracts/function.ligo" "2" in
  ok ()

let declarations () : unit result =
  let%bind program = type_file "./contracts/declarations.ligo" in
  Format.printf "toto\n%!" ;
  Printf.printf "toto\n%!" ;
  let aux n =
    let open AST_Typed.Combinators in
    let input = a_int n in
    let%bind result = easy_run_main_typed program input in
    let%bind result' =
      trace (simple_error "bad result") @@
      get_a_int result in
    Assert.assert_equal_int result' (42 + n)
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 42 ; 163 ; -1] in
  ok ()

let main = "Integration (End to End)", [
    test "basic" basic ;
    test "function" function_ ;
    test "declarations" declarations ;
  ]
