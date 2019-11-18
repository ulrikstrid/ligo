open Trace
open Test_helpers

let type_file = Ligo.Compile.Of_source.type_file (Syntax_name "pascaligo")

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/multisig.ligo" in
        s := Some program ;
        ok program
      )

open Ast_simplified


let keys =
  let open Tezos_crypto in
  let seed = Bigstring.of_string "00000000000000000000000000000000" in
  let (_,raw_pk,raw_sk) = Ed25519.generate_key ~seed () in
  fun () ->
    (raw_pk,raw_sk)

let str_keys =
  let open Tezos_crypto in
  let (raw_pk, raw_sk) = keys () in
  let (pk_str:string) = Base58.simple_encode (Ed25519.Public_key.b58check_encoding) raw_pk in
  let (sk_str:string) = Base58.simple_encode (Ed25519.Secret_key.b58check_encoding) raw_sk in
  fun () ->
    (pk_str,sk_str)

let sign_message (msg : expression) : string result =
  let open Tezos_crypto in
  let (_,raw_sk) = keys () in
  let (sk : Signature.secret_key) = Signature.Ed25519 raw_sk in
  let%bind program,_ = get_program () in
  let%bind (msg : Tezos_utils.Michelson.michelson) =
    let env = Ast_typed.program_environment program in
    Ligo.Run.Of_simplified.compile_expression ~env ~state:(Typer.Solver.initial_state) msg
  in
  let%bind msg' = Ligo.Run.Of_michelson.pack_message_lambda msg in
  let (signed_data:Signature.t) = Signature.sign sk msg' in
  let signature_str = Signature.to_b58check signed_data in
  ok signature_str

let init_storage is_valid threshold counter =
  let keys = 
    if is_valid then
      e_key @@ fst @@ str_keys ()
    else
      e_key "edpkteDwHwoNPB18tKToFKeSCykvr1ExnoMV5nawTJy9Y9nLTfQ541" in
  ez_e_record [
    ("counter" , e_nat counter ) ;
    ("threshold" , e_nat threshold) ;
    ("auth" , e_typed_list [ keys ] t_key ) ;
  ]

let msg = e_lambda "arguments"
  (Some t_unit) (Some (t_list t_operation))
  (e_typed_list [] t_operation)

let expected_return threshold counter =
  e_pair (e_typed_list [] t_operation) (init_storage true threshold counter)

let test_param is_valid counter = 
  let%bind signed_msg =
    if is_valid then 
      let%bind signature = sign_message msg in
      ok @@ e_signature signature
     else 
      ok @@ e_signature "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"
  in
  ok @@ e_constructor
    "CheckMessage"
    (ez_e_record [
      ("counter" , e_nat counter ) ;
      ("message" , msg) ;
      ("signatures" , e_typed_list [ signed_msg ; ] t_signature ) ;
    ])

let compile_main () = 
  let%bind program,_ = get_program () in
  let%bind () =
    Ligo.Run.Of_simplified.compile_program
    program "main" in
  ok ()

(* Provide one valid signature when the threshold is two *)
let not_enough_signature () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Not enough signatures passed the check" in
  let%bind test_params = test_param true 0 in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage true 2 0)) exp_failwith in
  ok ()

(* Provide one valid signature when the threshold is one with unmatching counters *)
let unmatching_counters () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Counters does not match" in
  let%bind test_params = test_param true 1 in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage true 1 0)) exp_failwith in
  ok ()

(* Provide one invalid signature when the threshold is one *)
let invalid_signature () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Invalid signature" in
  let%bind test_params = test_param false 0 in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage true 1 0)) exp_failwith in
  ok ()

(* Provide one valid signature when the threshold is one *)
let valid_signature () =
  let%bind program,_ = get_program () in
  let%bind () = expect_eq_n_trace_aux [0;1;2] program "main"
      (fun n ->
        let%bind params = test_param true n in
        ok @@ e_pair params (init_storage true 1 n)
      )
      (fun n -> ok @@ expected_return 1 (n+1)) in
  ok ()

let main = test_suite "Multisig" [
    test "compile"              compile_main         ;
    test "not_enough_signature" not_enough_signature ;
    test "unmatching_counters"  unmatching_counters  ;
    test "invalid_signature"    invalid_signature    ;
    test "valid_signature"      valid_signature      ;
  ]
