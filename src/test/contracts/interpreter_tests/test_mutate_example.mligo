let testme_test = "./testme.mligo"

let test =
  let init_storage = Test.compile_expression (Some testme_test) [%cameligo ({| (10 : int) |} : ligo_program) ] in
  let (addr, _, _) = Test.originate testme_test "main" init_storage in
  let prg : ligo_program = [%cameligo ({| Increment(30 + 2) |} : ligo_program)] in
  let prg : ligo_program = Test.mutate_expression prg in
  let _ = Test.log prg in
  let param = Test.compile_expression (Some testme_test) prg in
  let transfer_result = Test.transfer addr param 0n in
  let result = Test.get_storage addr in
  let check_ = Test.compile_expression (None : string option) [%cameligo ({| (42: int) |} : ligo_program)] in
  let _ = Test.log result in
  Test.michelson_equal result check_
