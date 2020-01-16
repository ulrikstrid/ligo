open Trace
open Test_helpers
open Ast_simplified

let type_file f =
  (* Change this line to use the name of your syntax, e.g. pascaligo, cameligo... *)
  let%bind simplified  = Ligo.Compile.Of_source.compile f (Syntax_name "cameligo") in
  let%bind typed,state = Ligo.Compile.Of_simplified.compile simplified in
  ok @@ (typed,state)

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        (* Change this line to use the relative path to your contract *)
        let%bind (program , state) = type_file "./contracts/access_control.mligo" in
        s := Some (program , state) ;
        ok (program , state)
      )

let compile_main () =
  (* Change this line to use the relative path to your contract and your syntax name *)
  let%bind simplified      = Ligo.Compile.Of_source.compile "./contracts/access_control.mligo" (Syntax_name "cameligo") in
  let%bind typed_prg,_ = Ligo.Compile.Of_simplified.compile simplified in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  (* Possibly change this line to use the entrypoint name of your contract *)
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

let (us_addr , us_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

(* Test this returns false when we're not owner or controller *)
let at_least_controller_not () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let controller_addr = addr 4 in
  let storage = e_ez_record [("owner", e_address owner_addr) ;
                             ("controller", e_address controller_addr)]
  in
  let%bind () = expect_eq program "at_least_controller"
    (e_pair (e_address us_addr) storage)
    (e_bool false)
  in ok ()

(* Test this returns true when we're controller *)
let at_least_controller_c () =
  let%bind program, _ = get_program () in
  let owner_addr = addr 5 in
  let controller_addr = us_addr in
  let storage = e_ez_record [("owner", e_address owner_addr) ;
                             ("controller", e_address controller_addr)]
  in
  let%bind () = expect_eq program "at_least_controller"
    (e_pair (e_address us_addr) storage)
    (e_bool true)
  in ok ()

(* Test this returns true when we're owner *)
let at_least_controller_owner () =
  let%bind program, _ = get_program () in
  let owner_addr = us_addr in
  let controller_addr = addr 4 in
  let storage = e_ez_record [("owner", e_address owner_addr) ;
                             ("controller", e_address controller_addr)]
  in
  let%bind () = expect_eq program "at_least_controller"
    (e_pair (e_address us_addr) storage)
    (e_bool true)
  in ok ()

(* Change this line to use a name you'll recognize for your test suite *)
let main = test_suite "Access Control" [
    test "at_least_controller false when not" at_least_controller_not ;
    test "at_least_controller true when controller" at_least_controller_c ;
    test "at_least_controller true when owner" at_least_controller_owner ;
  ]
