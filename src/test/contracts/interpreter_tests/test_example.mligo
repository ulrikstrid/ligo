let cut = "./contract_under_test/contract_create.mligo"

let check_new_origination (src :address) : address =
  let last_origs = Test.last_originations () in
  match Map.find_opt src last_origs with
    | Some new_lst -> ( 
      let () = assert (List.length new_lst = 1n) in
      match new_lst with
      | new_acc::rst -> new_acc
      | [] -> (failwith "more than one originated account" : address)
    )
    | None -> (failwith "source did not originate anything" : address)

let test =
  let baker = Test.nth_bootstrap_account 0 in
  let src = Test.nth_bootstrap_account 1 in

  let init_storage = Test.compile_expression (Some cut) [%cameligo ({| (None: storage) |} : ligo_program) ] in
  let (addr, code, size) = Test.originate cut "main" init_storage in
  let () = assert (Test.michelson_equal init_storage (Test.get_storage addr)) in
  let () = assert (size < 300) in
  let new_account1 = check_new_origination src in
  
  let param = Test.compile_expression (Some cut) [%cameligo ({| Two |} : ligo_program)] in
  let () = Test.transfer_exn addr param 10n in
  let new_account2 = check_new_origination new_account1 in
  let new_storage = Test.get_storage addr in
  let expected_new_storage = Test.compile_expression_subst (Some cut)
    [%cameligo ({| Some $x |} : ligo_program) ]
    [ ("x", Test.compile_value new_account2) ]
  in
  let () = assert (Test.michelson_equal new_storage expected_new_storage) in


  let param = Test.compile_expression (Some cut) [%cameligo ({| One |} : ligo_program)] in
  match (Test.transfer addr param 10n : test_exec_result) with
  | Success -> (failwith "contract did not fail" : michelson_program)
  | Fail x -> (
    let x = (fun (x : test_exec_error) -> x) x in 
    match x with
    | Rejected reject_data ->
      let (v,addr) = reject_data in
      let () = assert (addr = new_account2) in
      let () = assert (addr = new_account2) in
      let () = assert (Test.michelson_equal v (Test.compile_value 111)) in
      v
    | Other -> (failwith "contract failed for another reason" : michelson_program)
  )

let test2 =
  // By default:
  //  - only 2 bootstrap accounts are created with a default amount of 4000000000000 mutez
  //  - the 1st and 2nd bootstrap accounts are used as baker and source respectively
  
  // You can change the default behavior by reseting the state:
  let number_of_account = 4n in
  let overide_default_amounts = [ 100n ; 100n ] in // the [i]th element of the list overwrite default balance of the [i]th account 
  let () = Test.reset_state number_of_account ([1n;2n]: nat list) in
  // And by setting the source in between calls to `Test.transfer` or `Test.originate`
  let bsa0 = (Test.nth_bootstrap_account 0) in
  let bsa1 = (Test.nth_bootstrap_account 1) in
  let bsa2 = (Test.nth_bootstrap_account 2) in
  let bsa3 = (Test.nth_bootstrap_account 3) in
  let () = Test.set_source bsa3 in
  let () = Test.set_baker bsa2 in
  // some balance tests:
  let tz = fun (n:nat) ->
    Test.compile_expression_subst (Some cut)
      [%cameligo ({| $x * 1mutez |} : ligo_program) ]
      [ ("x", Test.compile_value n) ]
  in
  let () = assert (Test.michelson_equal (Test.get_balance bsa0) (tz 1n)) in
  let () = assert (Test.michelson_equal (Test.get_balance bsa1) (tz 2n)) in
  let () = assert (Test.michelson_equal (Test.get_balance bsa2) (tz 4000000000000n)) in
  let () = assert (Test.michelson_equal (Test.get_balance bsa3) (tz 4000000000000n)) in
  ()

