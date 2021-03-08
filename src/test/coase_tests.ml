(* Copyright Coase, Inc 2019 *)

open Trace
open Test_helpers
open Main_errors


let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
      let options = Compiler_options.make () in
      let%bind program  = Ligo.Compile.Utils.type_file ~options "./contracts/coase.ligo" "pascaligo" (Contract "main") in
      s := Some program;
      ok program
    )

let compile_main () = 
  let%bind typed_prg, _env, state = get_program () in
  let () = Typer.Solver.discard_state state in
  let%bind mini_c_prg         = Ligo.Compile.Of_typed.compile typed_prg in
  let%bind michelson_prg      = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract ~options mini_c_prg "main" in
  let%bind _contract =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

open Ast_imperative

let card owner =
  e_record_ez [
    ("card_owner" , owner) ;
    ("card_pattern" , e_nat 0) ;
  ]

let card_ty = t_record_ez [
    ("card_owner" , t_address ()) ;
    ("card_pattern" , t_nat ()) ;
  ]

let card_ez owner = card (e_address owner)

let make_cards assoc_lst =
  let card_id_ty = t_nat () in
  e_typed_map assoc_lst card_id_ty card_ty

let card_pattern (coeff , qtt) =
  e_record_ez [
    ("coefficient" , coeff) ;
    ("quantity" , qtt) ;
  ]

let card_pattern_ty =
  t_record_ez [
    ("coefficient" , t_tez ()) ;
    ("quantity" , t_nat ()) ;
  ]

let card_pattern_ez (coeff , qtt) =
  card_pattern (e_mutez coeff , e_nat qtt)

let make_card_patterns lst =
  let card_pattern_id_ty = t_nat () in
  let assoc_lst = List.mapi (fun i x -> (e_nat i , x)) lst in
  e_typed_map assoc_lst card_pattern_id_ty card_pattern_ty

let storage cards_patterns cards next_id =
  e_record_ez [
    ("cards" , cards) ;
    ("card_patterns" , cards_patterns) ;
    ("next_id" , next_id) ;
  ]

let storage_ez cps cs next_id =
  storage (make_card_patterns cps) (make_cards cs) (e_nat next_id)

let cards_ez owner n =
  List.mapi (fun i x -> (e_nat i , x))
  @@ List.map card_ez
  @@ List.map (Function.constant owner)
  @@ List.range n

let (first_owner , first_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let (second_owner , second_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 1 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let basic a b cards next_id =
  let card_patterns = List.map card_pattern_ez [
    (100 , a) ;
    (20 , b) ;
  ] in
  storage_ez card_patterns cards next_id

let buy () =
  let%bind program = get_program () in
  let%bind () =
    let make_input = fun n ->
      let buy_action = e_record_ez [
          ("card_to_buy" , e_nat 0) ;
        ] in
      let storage = basic 100 1000 (cards_ez first_owner n) (2 * n) in
      e_pair buy_action storage
    in
    let make_expected = fun n ->
      let ops = e_typed_list [] (t_operation ()) in
      let storage =
        let cards =
          cards_ez first_owner n @
          [(e_nat (2 * n) , card (e_address second_owner))]
        in
        basic 101 1000 cards ((2 * n) + 1) in
      e_pair ops storage
    in
    let%bind () =
      let%bind amount =
        trace_option (test_internal "getting amount for run") @@
          Memory_proto_alpha.Protocol.Alpha_context.Tez.of_mutez @@ Int64.of_int 10000000000 in
      let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~sender:second_contract () in
      expect_eq_n_pos_small ~options program "buy_single" make_input make_expected in
    let%bind () =
      let%bind amount =
        trace_option (test_internal "getting amount for run") @@
          Memory_proto_alpha.Protocol.Alpha_context.Tez.of_mutez @@ Int64.of_int 0 in
      let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~sender:second_contract () in
      Assert.assert_fail (test_internal "could buy without money") @@
        expect_eq_n_pos_small ~options program "buy_single" make_input make_expected in
    ok ()
  in
  ok ()

let dispatch_buy () =
  let%bind program = get_program () in
  let%bind () =
    let make_input = fun n ->
      let buy_action = e_record_ez [
          ("card_to_buy" , e_nat 0) ;
        ] in
      let action = e_constructor "Buy_single" buy_action in
      let storage = basic 100 1000 (cards_ez first_owner n) (2 * n) in
      e_pair action storage
    in
    let make_expected = fun n ->
      let ops = e_typed_list [] (t_operation ()) in
      let storage =
        let cards =
          cards_ez first_owner n @
          [(e_nat (2 * n) , card (e_address second_owner))]
        in
        basic 101 1000 cards ((2 * n) + 1) in
      e_pair ops storage
    in
    let%bind () =
      let%bind amount =
        trace_option (test_internal "getting amount for run") @@
        Memory_proto_alpha.Protocol.Alpha_context.Tez.of_mutez @@ Int64.of_int 10000000000 in
      let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~sender:second_contract () in
      expect_eq_n_pos_small ~options program "main" make_input make_expected in
    let%bind () =
      let%bind amount =
        trace_option (test_internal "getting amount for run") @@
        Memory_proto_alpha.Protocol.Alpha_context.Tez.of_mutez @@ Int64.of_int 0 in
      let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~sender:second_contract () in
      Assert.assert_fail (test_internal "could buy without money") @@
        expect_eq_n_pos_small ~options program "main" make_input make_expected in
    ok ()
  in
  ok ()

let transfer () =
  let%bind program = get_program () in
  let%bind () =
    let make_input = fun n ->
      let transfer_action = e_record_ez [
          ("card_to_transfer" , e_nat 0) ;
          ("destination" , e_address second_owner) ;
        ] in
      let storage = basic 100 1000 (cards_ez first_owner n) (2 * n) in
      e_pair transfer_action storage
    in
    let make_expected = fun n ->
      let ops = e_typed_list [] (t_operation ()) in
      let storage =
        let cards =
          let new_card = card_ez second_owner in
          let old_cards = cards_ez first_owner n in
          (e_nat 0 , new_card) :: (List.tl old_cards)
        in
        basic 100 1000 cards (2 * n) in
      e_pair ops storage
    in
    let%bind () =
      let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
      let sender = first_contract in
      let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~sender () in
      expect_eq_n_strict_pos_small ~options program "transfer_single" make_input make_expected in
    ok ()
  in
  ok ()

let sell () =
  let%bind program = get_program () in
  let%bind () =
    let make_input = fun n ->
      let sell_action = e_record_ez [
          ("card_to_sell" , e_nat (n - 1)) ;
        ] in
      let cards = cards_ez first_owner n in
      let storage = basic 100 1000 cards (2 * n) in
      e_pair sell_action storage
    in
    let make_expecter : int -> Ast_core.expression -> (unit,_) result = fun n result ->
      let%bind (ops , storage) = trace_option (test_internal __LOC__) @@
        Ast_core.get_e_pair result.content in
      let%bind () =
        let%bind lst = trace_option (test_internal __LOC__) @@
          Ast_core.get_e_list ops.content in
          Assert.assert_list_size (test_internal __LOC__) lst 1 in
      let expected_storage =
        let cards = List.hds @@ cards_ez first_owner n in
        basic 99 1000 cards (2 * n) in
      let%bind expected_storage = Test_helpers.expression_to_core expected_storage in
      trace_option (test_internal __LOC__) @@
        Ast_core.Misc.assert_value_eq (expected_storage , storage)
    in
    let%bind () =
      let amount = Memory_proto_alpha.Protocol.Alpha_context.Tez.zero in
      let sender = first_contract in
      let options = Proto_alpha_utils.Memory_proto_alpha.make_options ~amount ~sender () in
      expect_n_strict_pos_small ~options program "sell_single" make_input make_expecter in
    ok ()
  in
  ok ()


let main = test_suite "Coase (End to End)" [
    test "compile" compile_main ;
    test "buy" buy ;
    test "dispatch buy" dispatch_buy ;
    test "transfer" transfer ;
    test "sell" sell ;
  ]
