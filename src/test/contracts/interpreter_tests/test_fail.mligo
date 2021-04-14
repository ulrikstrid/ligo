let under_test = "./contract_under_test/fail_contract.mligo"

let test =
  let vunit = Test.compile_expression (None: string option) [%cameligo ({| () |} : ligo_program) ] in
  let vfail = Test.compile_expression (Some under_test) [%cameligo ({| fail_data |} : ligo_program) ] in
  let (addr,code,_) = Test.originate under_test "main" vunit in
  
  match Test.transfer addr vunit 10n with
  | Success -> (failwith "Should fail !" : michelson_program )
  | Fail e -> (
    match e with
    | Rejected x ->
      let (x, addr_fail) = x in
      let () = assert (addr_fail = addr) in
      x
    | Other -> (failwith "Failed, but wrong reason" : michelson_program )
  )
