open Trace
open Test_helpers
open Ast_simplified


let retype_file f =
  let%bind simplified  = Ligo.Compile.Of_source.compile f (Syntax_name "cameligo") in
  let%bind typed,state = Ligo.Compile.Of_simplified.compile simplified in
  ok (typed,state)

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = retype_file "./contracts/socks.mligo" in
        s := Some program ;
        ok program
      )

let compile_main () =
  let%bind simplified      = Ligo.Compile.Of_source.compile "./contracts/socks.mligo" (Syntax_name "cameligo") in
  let%bind typed_prg,_ = Ligo.Compile.Of_simplified.compile simplified in
  let%bind mini_c_prg      = Ligo.Compile.Of_typed.compile typed_prg in
  let%bind michelson_prg   = Ligo.Compile.Of_mini_c.aggregate_and_compile_contract mini_c_prg "main" in
  let%bind (_contract: Tezos_utils.Michelson.michelson) =
    (* fails if the given entry point is not a valid contract *)
    Ligo.Compile.Of_michelson.build_contract michelson_prg in
  ok ()

let (oracle_addr , oracle_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let (stranger_addr , stranger_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth dummy_environment.identities 1 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let empty_op_list =
  (e_typed_list [] t_operation)
let empty_message = e_lambda (Var.of_name "arguments")
  (Some t_unit) (Some (t_list t_operation))
  empty_op_list

let add_sock_type () =
  let%bind program, _ = get_program () in
  let new_sock_type = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "b890266c2ad86f7c12660e1b3700da5a70ad6ea3aa243c4cf501bdda4497424c")]
  in
  let sock_type_record_type = t_record_ez [("name", t_string);
                                            ("description_hash", t_string)] in
  let initial_sock_types = e_typed_big_map [] t_int sock_type_record_type in
  let initial_socks = e_typed_big_map [] t_int (t_record_ez [("sock_type", sock_type_record_type);
                                                             ("owner", t_address)])
  in
  let post_sock_types = e_typed_big_map [(e_int 0, new_sock_type)] t_int sock_type_record_type in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", initial_socks);
                                  ("sock_types", post_sock_types)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_tuple [e_int 0; new_sock_type] in
  expect_eq ~options program "add_sock_type"
    (e_pair parameter initial_storage)
    (e_pair (e_list []) post_storage)

let add_sock_type_unauthorized () =
  let%bind program, _ = get_program () in
  let new_sock_type = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "b890266c2ad86f7c12660e1b3700da5a70ad6ea3aa243c4cf501bdda4497424c")]
  in
  let sock_type_record_type = t_record_ez [("name", t_string);
                                            ("description_hash", t_string)] in
  let initial_sock_types = e_typed_big_map [] t_int sock_type_record_type in
  let initial_socks = e_typed_big_map [] t_int (t_record_ez [("sock_type", sock_type_record_type);
                                                             ("owner", t_address)])
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract ()
  in
  let parameter = e_tuple [e_int 0; new_sock_type] in
  expect_string_failwith ~options program "add_sock_type"
    (e_pair parameter initial_storage)
    "You are not the sock oracle."

let add_sock_type_exists () =
  let%bind program, _ = get_program () in
  let new_sock_type = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "b890266c2ad86f7c12660e1b3700da5a70ad6ea3aa243c4cf501bdda4497424c")]
  in
  let sock_type_record_type = t_record_ez [("name", t_string);
                                            ("description_hash", t_string)] in
  let socks = e_typed_big_map [] t_int (t_record_ez [("sock_type", sock_type_record_type);
                                                     ("owner", t_address)])
  in
  let sock_types = e_typed_big_map [(e_int 0, new_sock_type)] t_int sock_type_record_type in
  let storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                             ("socks", socks);
                             ("sock_types", sock_types)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_tuple [e_int 0; new_sock_type] in
  expect_string_failwith ~options program "add_sock_type"
    (e_pair parameter storage)
    "A sock type with this ID already exists."

let update_sock_type () =
  let%bind program, _ = get_program () in
  (* Hash says "A plain sock." *)
  let current_sock_type = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "b890266c2ad86f7c12660e1b3700da5a70ad6ea3aa243c4cf501bdda4497424c")]
  in
  (* Hash says "An ordinary cotton sock." *)
  let sock_type_update = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "a0f72c4cb3d015c2790bfe79e60a15b289042e56235e65ac3cd07817478cb5aa")]
  in
  let sock_type_record_type = t_record_ez [("name", t_string);
                                            ("description_hash", t_string)] in
  let initial_sock_types = e_typed_big_map
      [(e_int 0, current_sock_type)]
      t_int
      sock_type_record_type
  in
  let post_sock_types = e_typed_big_map
      [(e_int 0, sock_type_update)]
      t_int
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [] t_int (t_record_ez [("sock_type", sock_type_record_type);
                                                             ("owner", t_address)])
  in

  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", initial_socks);
                                  ("sock_types", post_sock_types)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_tuple [e_int 0; sock_type_update] in
  expect_eq ~options program "update_sock_type"
    (e_pair parameter initial_storage)
    (e_pair (e_list []) post_storage)

let update_sock_type_unauthorized () =
  let%bind program, _ = get_program () in
  (* Hash says "A plain sock." *)
  let current_sock_type = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "b890266c2ad86f7c12660e1b3700da5a70ad6ea3aa243c4cf501bdda4497424c")]
  in
  (* Hash says "An ordinary cotton sock." *)
  let sock_type_update = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "a0f72c4cb3d015c2790bfe79e60a15b289042e56235e65ac3cd07817478cb5aa")]
  in
  let sock_type_record_type = t_record_ez [("name", t_string);
                                            ("description_hash", t_string)] in
  let initial_sock_types = e_typed_big_map
      [(e_int 0, current_sock_type)]
      t_int
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [] t_int (t_record_ez [("sock_type", sock_type_record_type);
                                                             ("owner", t_address)])
  in

  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract ()
  in
  let parameter = e_tuple [e_int 0; sock_type_update] in
  expect_string_failwith ~options program "update_sock_type"
    (e_pair parameter initial_storage)
    "You are not the sock oracle."

let trade () =
  let%bind program, _ = get_program () in
  (* Hash says "A plain sock." *)
  let test_sock_type = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "b890266c2ad86f7c12660e1b3700da5a70ad6ea3aa243c4cf501bdda4497424c")]
  in
  let sock_type_record_type = t_record_ez [("name", t_string);
                                            ("description_hash", t_string)] in
  let initial_sock_types = e_typed_big_map
      [(e_int 0, test_sock_type)]
      t_int
      sock_type_record_type
  in
  let test_sock = e_record_ez [("sock_type", test_sock_type);
                               ("owner", e_address oracle_addr)]
  in
  let traded_sock = e_record_ez [("sock_type", test_sock_type);
                                 ("owner", e_address stranger_addr)]
  in
  let initial_socks = e_typed_big_map [(e_int 0, test_sock)]
      t_int (t_record_ez [("sock_type", sock_type_record_type);
                          ("owner", t_address)])
  in
  let post_socks = e_typed_big_map [(e_int 0, traded_sock)]
      t_int (t_record_ez [("sock_type", sock_type_record_type);
                          ("owner", t_address)])
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", post_socks);
                                  ("sock_types", initial_sock_types)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_tuple [e_int 0; e_address stranger_addr] in
  expect_eq ~options program "trade"
    (e_pair parameter initial_storage)
    (e_pair (e_list []) post_storage)

let trade_unauthorized () =
  let%bind program, _ = get_program () in
  (* Hash says "A plain sock." *)
  let test_sock_type = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "b890266c2ad86f7c12660e1b3700da5a70ad6ea3aa243c4cf501bdda4497424c")]
  in
  let sock_type_record_type = t_record_ez [("name", t_string);
                                            ("description_hash", t_string)] in
  let initial_sock_types = e_typed_big_map
      [(e_int 0, test_sock_type)]
      t_int
      sock_type_record_type
  in
  let test_sock = e_record_ez [("sock_type", test_sock_type);
                               ("owner", e_address oracle_addr)]
  in
  let initial_socks = e_typed_big_map [(e_int 0, test_sock)]
      t_int (t_record_ez [("sock_type", sock_type_record_type);
                          ("owner", t_address)])
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract ()
  in
  let parameter = e_tuple [e_int 0; e_address stranger_addr] in
  expect_string_failwith ~options program "trade"
    (e_pair parameter initial_storage)
    "You don't own this sock."

let trade_nonexistent () =
  let%bind program, _ = get_program () in
  (* Hash says "A plain sock." *)
  let test_sock_type = e_record_ez [
      ("name", e_string "Cotton Sock");
      ("description_hash",
       e_string "b890266c2ad86f7c12660e1b3700da5a70ad6ea3aa243c4cf501bdda4497424c")]
  in
  let sock_type_record_type = t_record_ez [("name", t_string);
                                            ("description_hash", t_string)] in
  let initial_sock_types = e_typed_big_map
      [(e_int 0, test_sock_type)]
      t_int
      sock_type_record_type
  in
  let test_sock = e_record_ez [("sock_type", test_sock_type);
                               ("owner", e_address oracle_addr)]
  in
  let initial_socks = e_typed_big_map [(e_int 0, test_sock)]
      t_int (t_record_ez [("sock_type", sock_type_record_type);
                          ("owner", t_address)])
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_tuple [e_int 10; e_address stranger_addr] in
  expect_string_failwith ~options program "trade"
    (e_pair parameter initial_storage)
    "There is no sock with that ID."

let main = test_suite "Socks" [
    test "add sock type" add_sock_type ;
    test "add sock type (unauthorized)" add_sock_type_unauthorized ;
    test "add sock type (fail if exists)" add_sock_type_exists ;
    test "update sock type" update_sock_type ;
    test "update sock type (unauthorized)" update_sock_type_unauthorized ;
    test "trade" trade ;
    test "trade (unauthorized)" trade_unauthorized ;
    test "trade (nonexistent sock ID)" trade_nonexistent ;
]
