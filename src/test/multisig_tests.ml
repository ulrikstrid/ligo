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

let init_storage threshold = ez_e_record [
    ("threshold" , e_nat threshold) ;
    ("auth" , e_typed_list [
      e_key "edpkteDwHwoNPB18tKToFKeSCykvr1ExnoMV5nawTJy9Y9nLTfQ541";
      ] t_key ) ;
  ]

let msg = e_lambda "arguments"
  (Some t_unit) (Some (t_list t_operation))
  (e_typed_list [] t_operation)

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

let compile_main () = 
  let%bind program,_ = get_program () in
  let%bind () =
    Ligo.Run.Of_simplified.compile_program
    program "main" in
  ok ()

let not_enough_signature () =
  let%bind _program = get_program () in

  (* let%bind _result = Ligo.Run.Of_simplified.error_from_typed_program
    program "main" (e_pair test_param (init_storage 3)) in *)
  (* let%bind _result = Ligo.Run.Of_simplified.run_typed_program
    program "main" (e_pair test_param (init_storage 3)) in *)
  (* fail @@ simple_error "FAKE ERROR" *)
  ok ()

let main = test_suite "Multisig" [
    test "compile" compile_main ;
    test "not_enough_signature" not_enough_signature ;
  ]
