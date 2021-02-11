open Errors
open Ast_typed
open Trace

module Operators_types = struct
  open Typesystem.Shorthands

  let tc_subarg   a b c = tc "arguments for (-)"        ~bound:[] ~constraints:[] ()
                                                        [a;b;c] [ [nat;nat;int] ;
                                                                  [nat;int;int] ;
                                                                  [int;nat;int] ;
                                                                  [int;int;int] ;
                                                                  [timestamp;int;timestamp] ;
                                                                  [timestamp;timestamp;int] ;
                                                                  [mutez;mutez;mutez] ;
                                                                ]
  let tc_sizearg  a     = tc "arguments for size"       ~bound:[] ~constraints:[] ()
                                                        [a]     [ [string] ; [bytes] ; let a = Location.wrap @@ P_variable (Var.fresh ()) in [list a] ]
  let tc_packable a     = tc "packable"                 ~bound:[] ~constraints:[] ()
                                                        [a]     [ [int] ; [string] ; [bool] ; [address] ; (*TODO…*) ]
  let tc_timargs  a b c = tc "arguments for ( * )"      ~bound:[] ~constraints:[] ()
                                                        [a;b;c] [ [nat;nat;nat] ; 
                                                                  [nat;int;int] ; 
                                                                  [int;nat;int] ;
                                                                  [int;int;int] ;
                                                                  [mutez;nat;mutez] ;
                                                                  [nat;mutez;mutez] ;
                                                                  (* bl struff todo ..*)
                                                                ]
  let tc_edivargs a b c d = tc "arguments and return values for ediv, div and mod"
                                                        ~bound:[] ~constraints:[] ()
                                                        [a;b;c;d] [ [nat;nat;nat;nat] ; 
                                                                    [nat;int;int;nat] ;
                                                                    [int;nat;int;nat] ;
                                                                    [int;int;int;nat] ;
                                                                    [mutez;nat;mutez;mutez] ;
                                                                    [mutez;mutez;nat;mutez] ;
                                                                  ]
  let tc_addargs  a b c = tc "arguments for (+)"        ~bound:[] ~constraints:[] ()
                                                        [a;b;c] [ [nat;nat;nat] ; 
                                                                  [int;int;int] ; 
                                                                  (* [nat;int;int] ;
                                                                  [int;nat;int] ; makes a bug in closure *)
                                                                  [timestamp;int;timestamp] ;
                                                                  [int;timestamp;int] ;
                                                                  [mutez;mutez;mutez]
                                                                  (* bls stuff *)
                                                                ]
  let tc_comparable a   = tc "comparable"               ~bound:[] ~constraints:[] ()
                                                        [a]     [ [int] ; 
                                                                  [nat] ; 
                                                                  [bool] ;
                                                                  [mutez] ; 
                                                                  [string] ;
                                                                  [bytes] ;
                                                                  [address] ;
                                                                  [timestamp] ;
                                                                  [key_hash] ;
                                                                  (* pair of comparable *)
                                                                ]
  (* TODO: enabling this makes some tests fail:
  let tc_concatable a b = tc "concatenable"             ~bound:[] ~constraints:[] ()
                                                        [a;b]   [ [tuple2 string string  ; string ] ;
                                                                  [tuple1 @@ list string ; string ] ; 
                                                                  [tuple2 bytes  bytes   ; bytes  ] ;
                                                                  [tuple1 @@ list bytes  ; bytes  ] ;
                                                                ] *)
  let tc_concatable a   = tc "concatenable"             ~bound:[] ~constraints:[] ()
                                                        [a]     [ [string] ; [bytes] ]
  let tc_slicable   a   = tc "slicable"                 ~bound:[] ~constraints:[] ()
                                                        [a]     [ [string] ; [bytes] ]
  let tc_storable a     = tc "storable"                 ~bound:[] ~constraints:[] ()
                                                        [a]     [ [string] ; [bytes] ; (*Humm .. TODO ?*) ]
  let tc_failwith a     = tc "failwith"                 ~bound:[] ~constraints:[] ()
                                                        [a]     [ [string] ; [int] ]
  let tc_bitwise a b c  = tc "bitwise"                  ~bound:[] ~constraints:[] ()
                                                        [a;b;c] [ [nat;nat;nat] ;
                                                                  [bool;bool;bool] ;
                                                                ]

  let t_none         = forall "a" @@ fun a -> tuple0 --> option a

  let t_sub          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_subarg a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_some         = forall "a" @@ fun a -> tuple1 a --> option a
  let t_map_remove   = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> map src dst
  let t_map_empty   =  forall2 "src" "dst" @@ fun src dst -> tuple0 --> map src dst
  let t_big_map_empty   =  forall2 "src" "dst" @@ fun src dst -> tuple0 --> big_map src dst
  let t_map_add      = forall2 "src" "dst" @@ fun src dst -> tuple3 src dst (map src dst) --> map src dst
  let t_map_update   = forall2 "src" "dst" @@ fun src dst -> tuple3 src (option dst) (map src dst) --> map src dst
  let t_map_mem      = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> bool
  let t_map_find     = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> dst
  let t_map_find_opt = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> option dst
  let t_map_fold     = forall3 "src" "dst" "acc" @@ fun src dst acc -> tuple3 ( pair acc (pair src dst ) --> acc ) (map src dst) acc --> acc
  let t_map_map      = forall3 "k" "v" "result" @@ fun k v result -> tuple2 ((k * v) --> result) (map k v) --> map k result
  let t_map_get_and_update = forall2 "k" "v" @@ fun k v -> tuple3 k (option v) (map k v) --> tuple2 (option v) (map k v)
  let t_big_map_get_and_update = forall2 "k" "v" @@ fun k v -> tuple3 k (option v) (big_map k v) --> tuple2 (option v) (big_map k v)

  (* TODO: the type of map_map_fold might be wrong, check it. *)
  let t_map_map_fold = forall4 "k" "v" "acc" "dst" @@ fun k v acc dst -> tuple3 ( ((k * v) * acc) --> acc * dst ) (map k v) (k * v) --> (map k dst * acc)
  let t_map_iter     = forall2 "k" "v" @@ fun k v -> tuple2 ( (k * v) --> unit ) (map k v) --> unit
  let t_size         = forall_tc "c" @@ fun c -> [tc_sizearg c] => tuple1 c --> nat (* TYPECLASS *)
  let t_slice        = forall_tc "s" @@ fun s -> [tc_slicable s] => tuple3 nat nat s --> s
  let t_failwith     = forall2_tc "a" "b" @@ fun a b -> [tc_failwith b] => tuple1 b --> a
  let t_get_force    = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> dst
  let t_int          = tuple1 nat --> int
  let t_bytes_pack   = forall_tc "a" @@ fun a -> [tc_packable a] => tuple1 a --> bytes (* TYPECLASS *)
  let t_bytes_unpack = forall_tc "a" @@ fun a -> [tc_packable a] => tuple1 bytes --> option a (* TYPECLASS *)
  let t_hash256      = tuple1 bytes --> bytes
  let t_hash512      = tuple1 bytes --> bytes
  let t_blake2b      = tuple1 bytes --> bytes
  let t_hash_key     = tuple1 key --> key_hash
  let t_is_nat       = tuple1 int --> option nat
  let t_check_signature = tuple3 key signature bytes --> bool
  let t_chain_id     = tuple0 --> chain_id
  let t_sender       = tuple0 --> address
  let t_source       = tuple0 --> address
  let t_unit         = tuple0 --> unit
  let t_amount       = tuple0 --> mutez
  let t_balance      = tuple0 --> mutez
  let t_address      = forall "a" @@ fun a -> tuple1 a --> address
  let t_now          = tuple0 --> timestamp
  let t_transaction  = forall "a" @@ fun a -> tuple3 a mutez (contract a) --> operation
  let t_get_contract = forall2 "a" "addr" @@ fun a addr -> tuple1 addr --> contract a
  let t_get_contract_opt = forall2 "a" "addr" @@ fun a addr -> tuple1 addr --> option (contract a)
  let t_get_entrypoint = forall3 "a" "entry" "addr" @@ fun a entry addr ->tuple2 entry addr --> contract a
  let t_get_entrypoint_opt = forall3 "a" "entry" "addr" @@ fun a entry addr ->tuple2 entry addr --> option (contract a)
  let t_abs          = tuple1 int --> nat
  let t_cons         = forall "a" @@ fun a -> tuple2 a (list a) --> list a
  let t_assertion    = tuple1 bool --> unit
  let t_assert_some  = forall "a" @@ fun a -> tuple1 (option a) --> unit
  let t_times        = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_timargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_ediv         = forall4_tc "a" "b" "c" "d" @@ fun a b c d -> [tc_edivargs a b c d] => tuple2 a b --> (option @@ tuple2 c d) (* TYPECLASS *)
  let t_div          = forall4_tc "a" "b" "c" "d" @@ fun a b c d -> [tc_edivargs a b c d] => tuple2 a b --> c (* TYPECLASS *)
  let t_mod          = forall4_tc "a" "b" "c" "d" @@ fun a b c d -> [tc_edivargs a b c d] => tuple2 a b --> d (* TYPECLASS *)
  let t_add          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_addargs a b c] => tuple2 a b --> c (* TYPECLASS *)
  let t_set_mem      = forall "a" @@ fun a -> tuple2 a (set a) --> bool
  let t_set_add      = forall "a" @@ fun a -> tuple2 a (set a) --> set a
  let t_set_remove   = forall "a" @@ fun a -> tuple2 a (set a) --> set a
  let t_not          = tuple1 bool --> bool

  let t_continuation  = forall "a" @@ fun a -> tuple1 a --> pair bool a
  let t_fold_while    = forall "a" @@ fun a -> tuple2 (a --> pair bool a) a --> a
  let t_neg           = tuple1 int --> int
  let t_and           = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_bitwise a b c] => tuple2 a b --> c
  let t_or            = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_bitwise a b c] => tuple2 a b --> c
  let t_xor           = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_bitwise a b c] => tuple2 a b --> c
  let t_lsl           = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_bitwise a b c] => tuple2 a b --> c
  let t_lsr           = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_bitwise a b c] => tuple2 a b --> c
  let t_comp          = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 a a --> bool
  (*  TODO: enabling this makes some tests fail
      let t_concat        = forall2_tc "a" "b" @@ fun a b -> [tc_concatable a b] => a --> b *)
  let t_concat        = forall_tc "a" @@ fun a -> [tc_concatable a] => tuple2 a a --> a

  let t_set_empty     = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple0 --> set a
  let t_set_iter      = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 (a --> unit) (set a) --> unit
  (* TODO: check that the implementation has this type *)
  let t_set_fold      = forall2_tc "a" "b" @@ fun a b -> [tc_comparable b] => tuple3 (pair a b --> a) (set b) a --> a
  let t_list_empty    = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple0 --> list a
  let t_list_iter     = forall "a" @@ fun a -> tuple2 (a --> unit) (list a) --> unit
  let t_list_map      = forall2 "a" "b" @@ fun a b -> tuple2 (a --> b) (list a) --> (list b)
  (* TODO: check that the implementation has this type *)
  let t_list_fold     = forall2 "a" "b" @@ fun a b -> tuple3 (pair a b --> a) (list b) a --> a
  let t_list_head_opt = forall "a" @@ fun a -> tuple1 (list a) --> option a
  let t_list_tail_opt = forall "a" @@ fun a -> tuple1 (list a) --> option (list a)
  let t_self_address  = tuple0 --> address
  let t_implicit_account = forall_tc "a" @@ fun a -> [tc_storable a] => tuple1 key_hash --> contract a
  let t_set_delegate  = tuple1 (option key_hash) --> operation

  let constant_type : constant' -> (Typesystem.Core.type_value, typer_error) result = function
    | C_INT                 -> ok @@ t_int ;
    | C_UNIT                -> ok @@ t_unit ;
    | C_NOW                 -> ok @@ t_now ;
    | C_IS_NAT              -> ok @@ t_is_nat ;
    | C_SOME                -> ok @@ t_some ;
    | C_NONE                -> ok @@ t_none ;
    | C_ASSERTION           -> ok @@ t_assertion ;
    | C_ASSERT_SOME         -> ok @@ t_assert_some ;
    | C_FAILWITH            -> ok @@ t_failwith ;
    (* LOOPS *)
    | C_FOLD_WHILE          -> ok @@ t_fold_while ;
    | C_FOLD_CONTINUE       -> ok @@ t_continuation ;
    | C_FOLD_STOP           -> ok @@ t_continuation ;
    (* MATH *)
    | C_NEG                 -> ok @@ t_neg ;
    | C_ABS                 -> ok @@ t_abs ;
    | C_ADD                 -> ok @@ t_add ;
    | C_SUB                 -> ok @@ t_sub ;
    | C_MUL                 -> ok @@ t_times ;
    | C_EDIV                -> ok @@ t_ediv ;
    | C_DIV                 -> ok @@ t_div ;
    | C_MOD                 -> ok @@ t_mod ;
    (* LOGIC *)
    | C_NOT                 -> ok @@ t_not ;
    | C_AND                 -> ok @@ t_and ;
    | C_OR                  -> ok @@ t_or ;
    | C_XOR                 -> ok @@ t_xor ;
    | C_LSL                 -> ok @@ t_lsl ;
    | C_LSR                 -> ok @@ t_lsr ;
    (* COMPARATOR *)
    | C_EQ                  -> ok @@ t_comp ;
    | C_NEQ                 -> ok @@ t_comp ;
    | C_LT                  -> ok @@ t_comp ;
    | C_GT                  -> ok @@ t_comp ;
    | C_LE                  -> ok @@ t_comp ;
    | C_GE                  -> ok @@ t_comp ;
    (* BYTES / STRING *)
    | C_SIZE                -> ok @@ t_size ;
    | C_CONCAT              -> ok @@ t_concat ;
    | C_SLICE               -> ok @@ t_slice ;
    | C_BYTES_PACK          -> ok @@ t_bytes_pack ;
    | C_BYTES_UNPACK        -> ok @@ t_bytes_unpack ;
    (* SET  *)
    | C_SET_EMPTY           -> ok @@ t_set_empty ;
    | C_SET_ADD             -> ok @@ t_set_add ;
    | C_SET_REMOVE          -> ok @@ t_set_remove ;
    | C_SET_ITER            -> ok @@ t_set_iter ;
    | C_SET_FOLD            -> ok @@ t_set_fold ;
    | C_SET_MEM             -> ok @@ t_set_mem ;

    (* LIST *)
    | C_CONS                -> ok @@ t_cons ;
    | C_LIST_EMPTY          -> ok @@ t_list_empty ;
    | C_LIST_ITER           -> ok @@ t_list_iter ;
    | C_LIST_MAP            -> ok @@ t_list_map ;
    | C_LIST_FOLD           -> ok @@ t_list_fold ;
    | C_LIST_HEAD_OPT       -> ok @@ t_list_head_opt ;
    | C_LIST_TAIL_OPT       -> ok @@ t_list_tail_opt ;

    (* MAP *)
    | C_MAP_EMPTY           -> ok @@ t_map_empty ;
    | C_BIG_MAP_EMPTY       -> ok @@ t_big_map_empty ;
    | C_MAP_ADD             -> ok @@ t_map_add ;
    | C_MAP_REMOVE          -> ok @@ t_map_remove ;
    | C_MAP_UPDATE          -> ok @@ t_map_update ;
    | C_MAP_ITER            -> ok @@ t_map_iter ;
    | C_MAP_MAP             -> ok @@ t_map_map ;
    | C_MAP_FOLD            -> ok @@ t_map_fold ;
    | C_MAP_MEM             -> ok @@ t_map_mem ;
    | C_MAP_FIND            -> ok @@ t_map_find ;
    | C_MAP_FIND_OPT        -> ok @@ t_map_find_opt ;
    | C_MAP_GET_AND_UPDATE  -> ok @@ t_map_get_and_update ;
    (* BIG MAP *)
    | C_BIG_MAP_GET_AND_UPDATE -> ok @@ t_big_map_get_and_update ;
    (* CRYPTO *)
    | C_SHA256              -> ok @@ t_hash256 ;
    | C_SHA512              -> ok @@ t_hash512 ;
    | C_BLAKE2b             -> ok @@ t_blake2b ;
    | C_HASH_KEY            -> ok @@ t_hash_key ;
    | C_CHECK_SIGNATURE     -> ok @@ t_check_signature ;
    | C_CHAIN_ID            -> ok @@ t_chain_id ;
    (*BLOCKCHAIN *)
    | C_CONTRACT            -> ok @@ t_get_contract ;
    | C_CONTRACT_OPT        -> ok @@ t_get_contract_opt ;
    | C_CONTRACT_ENTRYPOINT -> ok @@ t_get_entrypoint ;
    | C_CONTRACT_ENTRYPOINT_OPT -> ok @@ t_get_entrypoint_opt ;
    | C_AMOUNT              -> ok @@ t_amount ;
    | C_BALANCE             -> ok @@ t_balance ;
    | C_CALL                -> ok @@ t_transaction ;
    | C_SENDER              -> ok @@ t_sender ;
    | C_SOURCE              -> ok @@ t_source ;
    | C_ADDRESS             -> ok @@ t_address ;
    | C_SELF_ADDRESS        -> ok @@ t_self_address;
    | C_IMPLICIT_ACCOUNT    -> ok @@ t_implicit_account;
    | C_SET_DELEGATE        -> ok @@ t_set_delegate ;
    | c                     -> fail (corner_case (Format.asprintf "Typer not implemented for constant %a" Ast_typed.PP.constant' c))
end
