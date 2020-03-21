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
  let initial_sock_types = e_typed_big_map [] t_nat sock_type_record_type in
  let socks = e_typed_big_map [] (t_tuple [t_address; t_nat]) t_nat in
  let post_sock_types = e_typed_big_map [(e_nat 0, new_sock_type)] t_nat sock_type_record_type in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", socks);
                                  ("sock_types", post_sock_types);
                                  ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_tuple [e_nat 0; new_sock_type] in
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
  let initial_sock_types = e_typed_big_map [] t_nat sock_type_record_type in
  let socks = e_typed_big_map [] (t_tuple [t_address; t_nat]) t_nat in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract ()
  in
  let parameter = e_tuple [e_nat 0; new_sock_type] in
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
  let socks = e_typed_big_map [] (t_tuple [t_address; t_nat]) t_nat in
  let sock_types = e_typed_big_map [(e_nat 0, new_sock_type)] t_nat sock_type_record_type in
  let storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                             ("socks", socks);
                             ("sock_types", sock_types);
                             ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_tuple [e_nat 0; new_sock_type] in
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
      [(e_nat 0, current_sock_type)]
      t_nat
      sock_type_record_type
  in
  let post_sock_types = e_typed_big_map
      [(e_nat 0, sock_type_update)]
      t_nat
      sock_type_record_type
  in
  let socks = e_typed_big_map [] (t_tuple [t_address; t_nat]) t_nat in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", socks);
                                  ("sock_types", post_sock_types);
                                  ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_tuple [e_nat 0; sock_type_update] in
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
      [(e_nat 0, current_sock_type)]
      t_nat
      sock_type_record_type
  in
  let socks = e_typed_big_map [] (t_tuple [t_address; t_nat]) t_nat in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract ()
  in
  let parameter = e_tuple [e_nat 0; sock_type_update] in
  expect_string_failwith ~options program "update_sock_type"
    (e_pair parameter initial_storage)
    "You are not the sock oracle."

let add_sock () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map []
      (t_tuple [t_address; t_nat]) t_nat
  in
  let post_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", post_socks);
                                  ("sock_types", initial_sock_types);
                                  ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_nat 0 in
  expect_eq ~options program "add_sock"
    (e_pair parameter initial_storage)
    (e_pair (e_list []) post_storage)

let add_sock_unauthorized () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [] (t_tuple [t_address; t_nat]) t_nat in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract ()
  in
  let parameter = e_nat 0 in
  expect_string_failwith ~options program "add_sock"
    (e_pair parameter initial_storage)
    "You are not the sock oracle."

let buy_stock () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let post_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 0);
                                    (e_tuple [e_address stranger_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", post_socks);
                                  ("sock_types", initial_sock_types);
                                  ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let parameter = e_nat 0 in
  expect_eq ~options program "buy_stock"
    (e_pair parameter initial_storage)
    (e_pair (e_list []) post_storage)

(* Interesting case where the oracle buys a sock from itself, leaving it still in stock *)
let buy_stock_self_buy () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", socks);
                                  ("sock_types", initial_sock_types);
                                  ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let parameter = e_nat 0 in
  expect_eq ~options program "buy_stock"
    (e_pair parameter initial_storage)
    (e_pair (e_list []) post_storage)

(* Make sure we fail if attempt to buy stock that doesn't exist *)
let buy_stock_nonexistent () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let parameter = e_nat 10 in
  expect_string_failwith ~options program "buy_stock"
    (e_pair parameter initial_storage)
    "This sock is not in the oracle's stock (Tip: Wrong ID?)."

(* Make sure we fail when a sock type is out of stock *)
let buy_stock_oos () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 0)]
      (t_tuple [t_address ; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ()
  in
  let parameter = e_nat 0 in
  expect_string_failwith ~options program "buy_stock"
    (e_pair parameter initial_storage)
    "This sock is not in the oracle's stock (Tip: Out Of Stock)."

(* Pay too little for a sock *)
let buy_stock_underpay () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address ; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract
      ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.zero) ()
  in
  let parameter = e_nat 0 in
  expect_string_failwith ~options program "buy_stock"
    (e_pair parameter initial_storage)
    "You paid too little for your sock."

let transfer () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let post_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 0);
                                    (e_tuple [e_address stranger_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", post_socks);
                                  ("sock_types", initial_sock_types);
                                  ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_list [e_record_ez [("from_", e_address oracle_addr);
                                       ("to_", e_address stranger_addr);
                                       ("token_id", e_nat 0);
                                       ("amount", e_nat 1);]] in
  expect_eq ~options program "transfer"
    (e_pair parameter initial_storage)
    (e_pair (e_list []) post_storage)

let transfer_self () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let post_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                  ("socks", socks);
                                  ("sock_types", initial_sock_types);
                                  ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_list [e_record_ez [("from_", e_address oracle_addr);
                                       ("to_", e_address oracle_addr);
                                       ("token_id", e_nat 0);
                                       ("amount", e_nat 1);]] in
  expect_eq ~options program "transfer"
    (e_pair parameter initial_storage)
    (e_pair (e_list []) post_storage)

let transfer_unauthorized () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:stranger_contract ()
  in
  let parameter =  e_list [e_record_ez [("from_", e_address oracle_addr);
                                       ("to_", e_address stranger_addr);
                                       ("token_id", e_nat 0);
                                       ("amount", e_nat 1);]] in
  expect_string_failwith ~options program "transfer"
    (e_pair parameter initial_storage)
    "You don't own a sock of that type."

let transfer_nonexistent () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_list [e_record_ez [("from_", e_address oracle_addr);
                                       ("to_", e_address stranger_addr);
                                       ("token_id", e_nat 10);
                                       ("amount", e_nat 1);]] in
  expect_string_failwith ~options program "transfer"
    (e_pair parameter initial_storage)
    "You don't own a sock of that type."

let transfer_overdraw () =
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
      [(e_nat 0, test_sock_type)]
      t_nat
      sock_type_record_type
  in
  let initial_socks = e_typed_big_map [(e_tuple [e_address oracle_addr; e_nat 0], e_nat 1)]
      (t_tuple [t_address; t_nat]) t_nat
  in
  let initial_storage = e_record_ez [("sock_oracle", e_address oracle_addr);
                                     ("socks", initial_socks );
                                     ("sock_types", initial_sock_types);
                                     ("stock_price", e_mutez 1000000)]
  in
  let options = Proto_alpha_utils.Memory_proto_alpha.make_options
      ~sender:oracle_contract ()
  in
  let parameter = e_list [e_record_ez [("from_", e_address oracle_addr);
                                       ("to_", e_address stranger_addr);
                                       ("token_id", e_nat 0);
                                       ("amount", e_nat 10);]] in
  expect_string_failwith ~options program "transfer"
    (e_pair parameter initial_storage)
    "Attempted to transfer more socks of that type than you have."

let main = test_suite "Socks" [
    test "add sock type" add_sock_type ;
    test "add sock type (unauthorized)" add_sock_type_unauthorized ;
    test "add sock type (fail if exists)" add_sock_type_exists ;
    test "update sock type" update_sock_type ;
    test "update sock type (unauthorized)" update_sock_type_unauthorized ;
    test "add sock" add_sock ;
    test "add sock (unauthorized)" add_sock_unauthorized ;
    test "buy stock" buy_stock ;
    test "buy stock (oracle buys from self)" buy_stock_self_buy ;
    test "buy stock (fail if sock ID nonexistent)" buy_stock_nonexistent ;
    test "buy stock (fail if sock not in stock)" buy_stock_oos ;
    test "buy stock (underpayment)" buy_stock_underpay ;
    test "transfer" transfer ;
    test "transfer (to self)" transfer_self ;
    test "transfer (unauthorized)" transfer_unauthorized ;
    test "transfer (nonexistent sock ID)" transfer_nonexistent ;
    test "transfer (overdraw)" transfer_overdraw ;
]
