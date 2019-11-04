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

let init_storage = ez_e_record [
    ("threshold" , e_nat 3) ;
    ("auth" , e_typed_list [
      e_key "edpkteDwHwoNPB18tKToFKeSCykvr1ExnoMV5nawTJy9Y9nLTfQ541"
      ] t_key ) ;
  ]

let msg = e_lambda "arguments"
  (Some t_unit) (Some (t_list t_operation))
  (e_list [])

let test_param = 
  e_constructor
    "CheckMessage"
    (ez_e_record [
      ("message" , msg) ;
      ("signatures" ,
        e_typed_list [
          e_signature "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q" ;
        ] t_signature
      ) ;
    ])

let init_vote () =
  let%bind program = get_program () in
  (* let _ = Format.printf "--> \n %a \n" Ast_typed.PP.program program in *)
  let%bind result = Ligo.Run.Of_simplified.run_typed_program
    program "main" (e_pair test_param init_storage) in
  let _ = Format.printf "--> \n %a \n" Ast_simplified.PP.expression result in
  (* let%bind (_ , storage) = extract_pair result in
  let%bind storage' = extract_record storage in
  let votes = List.assoc "candidates" storage' in
  let%bind votes' = extract_map votes in
  let%bind (_ , yess) =
    trace_option (simple_error "") @@
    List.find_opt (fun (k , _) -> Ast_simplified.Misc.is_value_eq (k , e_string "Yes")) votes' in
  let%bind () = Ast_simplified.Misc.assert_value_eq (yess , e_int 1) in *)
  ok ()

let main = test_suite "Multisig" [
    test "type" init_vote ;
  ]
