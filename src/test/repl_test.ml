open Test_helpers
open Trace

let dry_run_options = Proto_alpha_utils.Memory_proto_alpha.make_options ()
let init_state_cameligo = Repl.make_initial_state
                            Ligo_compile.Helpers.CameLIGO
                            Environment.Protocols.Edo
                            false dry_run_options

let init_state_jsligo = Repl.make_initial_state
                          Ligo_compile.Helpers.JsLIGO
                          Environment.Protocols.Edo
                          false dry_run_options

let apply_repl_sequence init_state commands =
  let f state command =
    let _,state,out = Repl.parse_and_eval (Ex_display_format Dev) state command in
    (state, out) in
  let _, trace = List.fold_map_acc f init_state commands in
  trace

let test_seq init_state cmds res () =
  let r = apply_repl_sequence init_state cmds in
  if (List.compare ~compare:String.compare res r = 0)
  then ok @@ ()
  else fail @@ `Test_repl (res, r)

let test_basic () =
  let _,_,s = Repl.parse_and_eval (Ex_display_format Dev) init_state_cameligo "1 + 3" in
  if (String.compare s "4" = 0)
  then ok @@ ()
  else fail @@ `Test_repl ([s], ["4"])

let test_def () =
  test_seq init_state_cameligo ["let f (x : int) = x * 2"; "f 3"]
                               ["f"; "6"]
                               ()

let test_mod () =
  test_seq init_state_cameligo ["module EURO = struct
    type t = int
    let add (a , b : t * t) : t = a + b
    let zero : t = 0
    let one : t = 1
   end"; "EURO.one"]
           ["EURO"; "1"]
           ()

let test_use () =
  test_seq init_state_cameligo ["#use \"contracts/build/A.mligo\""; "toto"]
           ["toto"; "1"]
           ()

let test_long () =
  test_seq init_state_cameligo ["#use \"contracts/build/A.mligo\"";
            "toto";
            "#import \"contracts/build/B.mligo\" \"MYMOD\"";
            "MYMOD.toto";
            "MYMOD.A.toto";
            "let f (x : int) = MYMOD.f (unit, x)";
            "f 4";
            "module EURO = struct
    type t = nat
    let add (a, b : t * t) : t = a + b
    module CONST = struct
        let zero : t = 0n
        let one : t = 1n
    end
end";
            "module US_DOLLAR = EURO";
            "US_DOLLAR.CONST.zero + 32n" ]
           ["toto";
            "1";
            "Done.";
            "32";
            "1";
            "f";
            "( LIST_EMPTY() , 48 )";
            "EURO";
            "US_DOLLAR";
            "+32"]
           ()

let test_basic_jsligo () =
  let _,_,s = Repl.parse_and_eval (Ex_display_format Dev) init_state_jsligo "1 + 3" in
  if (String.compare s "4" = 0)
  then ok @@ ()
  else fail @@ `Test_repl ([s], ["4"])

let test_def_jsligo () =
  test_seq init_state_jsligo ["let f = (x : int) : int => x * 2"; "f(3)"]
                             ["f"; "6"]
                             ()

let test_mod_jsligo () =
  test_seq init_state_jsligo ["namespace EURO {
    export type t = int;
    export let add = ([a, b]: [t, t]): t => a + b;
    export let zero: t = 0;
    export let one: t = 1
}"; "EURO.one"]
           ["EURO"; "1"]
           ()

let test_use_jsligo () =
  test_seq init_state_jsligo ["#use \"contracts/build/A.mligo\""; "toto"]
           ["toto"; "1"]
           ()

let test_long_jsligo () =
  test_seq init_state_jsligo ["#use \"contracts/build/A.jsligo\"";
            "toto";
            "#import \"contracts/build/B.jsligo\" \"MYMOD\"";
            "MYMOD.toto";
            "MYMOD.A.toto";
            "let g = (x : int) : [list<operation>, int] => MYMOD.f (unit, x)";
            "g(4)";
            "namespace EURO {
    export type t = nat;
    export let add = ([a, b]: [t, t]): t => a + b;
    export namespace CONST {
        export let zero: t = 0 as nat;
        export let one: t = 1 as nat;
    };
}";
            "import US_DOLLAR = EURO";
            "US_DOLLAR.CONST.zero + (32 as nat)" ]
           ["toto";
            "1";
            "Done.";
            "32";
            "1";
            "g";
            "( LIST_EMPTY() , 48 )";
            "EURO";
            "US_DOLLAR";
            "+32"]
           ()

let () =
  Printexc.record_backtrace true ;
  run_test @@ test_suite "LIGO" [
    test_suite "REPL (cameligo)" [
        test "basic" test_basic;
        test "def&eval" test_def;
        test "mod" test_mod;
        test "use" test_use;
        test "long" test_long
      ] ;
    test_suite "REPL (jsligo)" [
        test "basic" test_basic_jsligo;
        test "def&eval" test_def_jsligo;
        test "mod" test_mod_jsligo;
        test "use" test_use_jsligo;
        test "long" test_long_jsligo
      ]

    ] ;
  ()
