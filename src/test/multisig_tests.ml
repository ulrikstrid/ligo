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
  let%bind (msg : Tezos_utils.Michelson.michelson) =
    Compile.Of_simplified.get_lambda_code ~state:(Typer.Solver.initial_state) msg in
  let%bind msg' = Ligo.Run.Of_michelson.pack_message_lambda msg in
  let (signed_data:Signature.t) = Signature.sign sk msg' in
  let signature_str = Signature.to_b58check signed_data in
  ok signature_str

let init_storage threshold =
  let (pk,_) = str_keys () in
  let _ = Format.printf "\n ----> %s\n" pk in
  ez_e_record [
    ("counter" , e_nat 0 ) ;
    ("threshold" , e_nat threshold) ;
    ("auth" , e_typed_list [
      (* e_key "edpkteDwHwoNPB18tKToFKeSCykvr1ExnoMV5nawTJy9Y9nLTfQ541"; *)
      e_key pk
      ] t_key ) ;
  ]

let msg = e_lambda "arguments"
  (Some t_unit) (Some (t_list t_operation))
  (e_typed_list [] t_operation)

let test_param () = 
  let%bind signed_msg = sign_message msg in
  let _ = Format.printf "\n----> %s\n" signed_msg in
  ok @@ e_constructor
    "CheckMessage"
    (ez_e_record [
      ("counter" , e_nat 0 ) ;
      ("message" , msg) ;
      ("signatures" ,
        e_typed_list [
          e_signature signed_msg ;
          (* e_signature "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q" ; *)
        ] t_signature
      ) ;
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
  let%bind test_params = test_param () in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 2)) exp_failwith in
  ok ()

(* Provide one invalid signature when the threshold is one *)
let invalid_signature () =
  let%bind program,_ = get_program () in
  let exp_failwith = "Invalid signature" in
  let%bind test_params = test_param () in
  let%bind () = expect_string_failwith
    program "main" (e_pair test_params (init_storage 1)) exp_failwith in
  ok ()

let main = test_suite "Multisig" [
    test "compile"              compile_main         ;
    test "not_enough_signature" not_enough_signature ;
    test "invalid_signature"    invalid_signature    ;
  ]
