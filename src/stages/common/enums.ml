type z = Z.t
type ligo_string = Simple_utils.Ligo_string.t

let [@warning "-32"] z_to_yojson x = `String (Z.to_string x)
let [@warning "-32"] z_of_yojson x =
  try match x with
      | `String s -> Ok (Z.of_string s)
      | _ -> Utils.error_yojson_format "JSON string"
  with
  | Invalid_argument _ ->
     Error "Invalid formatting.
            The Zarith library does not know how to handle this formatting."

let bytes_to_yojson b = `String (Bytes.to_string b)

type layout =
  | L_comb
  | L_tree

type literal =
  | Literal_unit
  | Literal_int of z
  | Literal_nat of z
  | Literal_timestamp of z
  | Literal_mutez of z
  | Literal_string of ligo_string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_signature of string
  | Literal_key of string
  | Literal_key_hash of string
  | Literal_chain_id of string
  | Literal_operation of bytes

type constant' =
  | C_INT
  | C_UNIT
  | C_NIL
  | C_NOW
  | C_IS_NAT
  | C_SOME
  | C_NONE
  | C_ASSERTION
  | C_ASSERT_SOME
  | C_ASSERT_INFERRED
  | C_FAILWITH
  | C_UPDATE
  (* Loops *)
  | C_ITER
  | C_FOLD_WHILE
  | C_FOLD_CONTINUE
  | C_FOLD_STOP
  | C_LOOP_LEFT
  | C_LOOP_CONTINUE
  | C_LOOP_STOP
  | C_FOLD
  (* MATH *)
  | C_NEG
  | C_ABS
  | C_ADD
  | C_SUB
  | C_MUL
  | C_EDIV
  | C_DIV
  | C_MOD
  (* LOGIC *)
  | C_NOT
  | C_AND
  | C_OR
  | C_XOR
  | C_LSL
  | C_LSR
  (* COMPARATOR *)
  | C_EQ
  | C_NEQ
  | C_LT
  | C_GT
  | C_LE
  | C_GE
  (* Bytes/ String *)
  | C_SIZE
  | C_CONCAT
  | C_SLICE
  | C_BYTES_PACK
  | C_BYTES_UNPACK
  | C_CONS
  (* Pair *)
  | C_PAIR
  | C_CAR
  | C_CDR
  | C_TRUE
  | C_FALSE
  | C_LEFT
  | C_RIGHT
  (* Set *)
  | C_SET_EMPTY
  | C_SET_LITERAL
  | C_SET_ADD
  | C_SET_REMOVE
  | C_SET_ITER
  | C_SET_FOLD
  | C_SET_MEM
  | C_SET_UPDATE
  (* List *)
  | C_LIST_EMPTY
  | C_LIST_LITERAL
  | C_LIST_ITER
  | C_LIST_MAP
  | C_LIST_FOLD
  | C_LIST_HEAD_OPT
  | C_LIST_TAIL_OPT
  (* Maps *)
  | C_MAP
  | C_MAP_EMPTY
  | C_MAP_LITERAL
  | C_MAP_GET
  | C_MAP_GET_FORCE
  | C_MAP_ADD
  | C_MAP_REMOVE
  | C_MAP_UPDATE
  | C_MAP_ITER
  | C_MAP_MAP
  | C_MAP_FOLD
  | C_MAP_MEM
  | C_MAP_FIND
  | C_MAP_FIND_OPT
  | C_MAP_GET_AND_UPDATE
  (* Big Maps *)
  | C_BIG_MAP
  | C_BIG_MAP_EMPTY
  | C_BIG_MAP_LITERAL
  | C_BIG_MAP_GET_AND_UPDATE
  (* Crypto *)
  | C_SHA256
  | C_SHA512
  | C_BLAKE2b
  | C_HASH
  | C_HASH_KEY
  | C_CHECK_SIGNATURE
  | C_CHAIN_ID
  (* Blockchain *)
  | C_CALL
  | C_CONTRACT
  | C_CONTRACT_OPT
  | C_CONTRACT_ENTRYPOINT
  | C_CONTRACT_ENTRYPOINT_OPT
  | C_AMOUNT
  | C_BALANCE
  | C_SOURCE
  | C_SENDER
  | C_ADDRESS
  | C_SELF
  | C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT
  | C_SET_DELEGATE
  | C_CREATE_CONTRACT
  | C_CONVERT_TO_LEFT_COMB
  | C_CONVERT_TO_RIGHT_COMB
  | C_CONVERT_FROM_LEFT_COMB
  | C_CONVERT_FROM_RIGHT_COMB
  (* Tests - ligo interpreter only *)
  | C_TEST_ORIGINATE
  | C_TEST_GET_STORAGE
  | C_TEST_GET_BALANCE
  | C_TEST_SET_NOW
  | C_TEST_SET_SOURCE
  | C_TEST_SET_BALANCE
  | C_TEST_EXTERNAL_CALL
  | C_TEST_ASSERT_FAILURE
  | C_TEST_LOG
  (* New with EDO*)
  | C_SHA3
  | C_KECCAK
  | C_LEVEL
  | C_VOTING_POWER
  | C_TOTAL_VOTING_POWER
  | C_TICKET
  | C_READ_TICKET
  | C_SPLIT_TICKET
  | C_JOIN_TICKET
  | C_PAIRING_CHECK
  | C_SAPLING_VERIFY_UPDATE
  | C_SAPLING_EMPTY_STATE

type deprecated = {
    name : string ;
    const : constant' ;
  }

type rich_constant =
  | Deprecated of deprecated
  | Const of constant'
