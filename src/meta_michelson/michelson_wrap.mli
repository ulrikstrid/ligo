open Proto_alpha_utils.Memory_proto_alpha
module AC = Alpha_context

module Types = Contract.Types
module Option = Simple_utils.Option
module MBytes = Alpha_environment.MBytes

module Stack : sig
  open Script_typed_ir

  type nonrec 'a ty = 'a ty
  type 'a t = 'a stack_ty
  type nonrec ('a, 'b) descr = ('a, 'b) descr
  type ('a, 'b) code = ('a t) -> ('a, 'b) descr

  type ex_stack_ty = Ex_stack_ty : 'a t -> ex_stack_ty
  type ex_descr = Ex_descr : ('a, 'b) descr -> ex_descr
  type ex_code = Ex_code : ('a, 'b) code -> ex_code

  val nil : unit t
  val stack : ?annot:var_annot -> 'a ty -> 'b t -> ('a * 'b) t
  val (@>) : 'a t -> ('a, 'a) code -> ('a, 'a) descr
  val (@:) : ?annot:var_annot ->'a ty -> 'b t -> ('a * 'b) t
  val (!:) : ('a, 'b) descr -> ('a, 'b) code

  (*
  val descr : 'a stack_ty -> 'b stack_ty -> ('a, 'b) instr -> ('a, 'b) descr
  val head : ( 'a * 'b ) t -> 'a ty
  val tail : ( 'a * 'b ) t -> 'b t
  val seq : ('a t -> ('a, 'b) descr) -> ('b t -> ('b, 'c) descr) -> 'a t -> ('a, 'c) descr
  val (@|) : 
           ('a t -> ('a, 'b) descr) ->
           ('b t -> ('b, 'c) descr) -> 'a t -> ('a, 'c) descr
  val (<.) : 'a t -> ('a, 'b) code -> ('a, 'b) descr
  val (<::) : ('a, 'b) descr -> ('b, 'c) descr -> ('a, 'c) descr
  val (<:) : ('a, 'b) descr -> ('b, 'c) code -> ('a, 'c) descr
  *)

end

open Stack

type nat = AC.Script_int.n AC.Script_int.num
type int_num = AC.Script_int.z AC.Script_int.num
type bytes = MBytes.t
type address = AC.Contract.t Script_typed_ir.ty
type mutez = AC.Tez.t Script_typed_ir.ty


module Stack_ops : sig
  open Script_typed_ir

  val exec : ('a * (('a, 'b) lambda * 'c), 'b * 'c) code
  val push_none : 'a ty -> (' rest, 'a option * 'rest ) code
  val push_unit : ( 'rest, unit * 'rest ) code
  val push_int : int -> 'rest t -> ( 'rest , (int_num * 'rest ) ) descr
  val push_bool : bool -> ('s, bool * 's) code
  val push_generic : 'a ty ->  'a -> ('s, 'a * 's) code

  (*
  val dup :  ( 'a * 'rest, 'a * ( 'a * 'rest ) ) code
  val drop : ('a * 'rest, 'rest) code
  val swap : ('a * ('b * 'c)) t -> ('a * ('b * 'c), 'b * ('a * 'c)) descr
  val dip : 
      ('rest t -> ('rest, 'a) descr) ->
      ('ty * 'rest) t -> ('ty * 'rest, 'ty * 'a) descr
  val noop : ('r, 'r) code 
  val fail : 'b t -> ('a * 'r, 'b) code
  val push_string : string -> 'rest t -> ( 'rest , (string * 'rest)) descr
  val push_nat : int -> 'rest t -> ( 'rest, ( nat * 'rest ) ) descr
  val push_tez : int -> 'rest t -> ( 'rest , (AC.Tez.tez * 'rest ) ) descr
  val failstring : string -> 'a t -> 'b t -> ( 'b, 'a ) descr
  *)

end

module Stack_shortcuts : sig

  val diiiip : 
      ('a t -> ('a, 'b) descr) ->
      ('c * ('d * ('e * ('f * 'a)))) t ->
      ('c * ('d * ('e * ('f * 'a))), 'c * ('d * ('e * ('f * 'b)))) descr
  val bubble_1 : ('a * ('b * 'c)) t -> ('a * ('b * 'c), 'b * ('a * 'c)) descr
  val bubble_down_1 : ('a * ('b * 'c)) t -> ('a * ('b * 'c), 'b * ('a * 'c)) descr
  val bubble_down_2 : ('a * ('b * ('c * 'r)), ('b * ('c * ('a * 'r)))) code
  val bubble_3 : ('a * ('b * ('c * ('d * 'r))), 'd * ('a * ('b * ('c * 'r)))) code
  val save_1_1 : ('a * 'r, 'b * 'r) code -> ('a * 'r, 'b * ('a * 'r)) code
  val keep_2_1 : ('a * ('b * 'r), 's) code -> ('a * ('b * 'r), 'b * 's) code
  val relativize_1_1 : ('a * unit, 'b * unit) descr -> ('a * unit, 'b * unit) code

  (*
  val diip : 
      ('a t -> ('a, 'b) descr) ->
      ('c * ('d * 'a)) t ->
      ('c * ('d * 'a), 'c * ('d * 'b)) descr
  val diiip : 
      ('a t -> ('a, 'b) descr) ->
      ('c * ('d * ('e * 'a))) t ->
      ('c * ('d * ('e * 'a)), 'c * ('d * ('e * 'b))) descr
  val bubble_2 : ('a * ('b * ('c * 'r)), 'c * ('a * ('b * 'r))) code
  val keep_1 : ('a * 'r, 's) code -> ('a * 'r, 'a * 's) code
  val keep_2 : ('a * ('b * 'r), 's) code -> ('a * ('b * 'r), ('a * ('b * 's))) code
  *)

end

module Pair_ops : sig

  val pair : ('a * ('b * 'rest)) t -> ('a * ('b * 'rest), ('a, 'b) Memory_proto_alpha.Script_typed_ir.pair * 'rest) descr
  val carcdr : 
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair *
      (('c, 'd) Memory_proto_alpha.Script_typed_ir.pair * 'e)) t ->
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair *
      (('c, 'd) Memory_proto_alpha.Script_typed_ir.pair * 'e),
      'a * ('d * 'e)) descr
  val cdrcar :
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair *
      (('c, 'd) Memory_proto_alpha.Script_typed_ir.pair * 'e)) t ->
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair *
      (('c, 'd) Memory_proto_alpha.Script_typed_ir.pair * 'e),
      'b * ('c * 'e)) descr
  val cdrcdr :
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair *
      (('c, 'd) Memory_proto_alpha.Script_typed_ir.pair * 'e)) t ->
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair *
      (('c, 'd) Memory_proto_alpha.Script_typed_ir.pair * 'e),
      'b * ('d * 'e)) descr
  val carcar :
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair *
      (('c, 'd) Memory_proto_alpha.Script_typed_ir.pair * 'e)) t ->
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair *
      (('c, 'd) Memory_proto_alpha.Script_typed_ir.pair * 'e),
      'a * ('c * 'e)) descr
  val cdar :
      (('a, ('b, 'c) Memory_proto_alpha.Script_typed_ir.pair)
      Memory_proto_alpha.Script_typed_ir.pair * 'd) t ->
      (('a, ('b, 'c) Memory_proto_alpha.Script_typed_ir.pair)
      Memory_proto_alpha.Script_typed_ir.pair * 'd, 'b * 'd) descr
  val unpair :
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair * 'c) t ->
      (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair * 'c, 'a * ('b * 'c)) descr

  (*
  val car : (('a * 'b) * 'rest) t -> (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair * 'rest, 'a * 'rest) descr
  val cdr : (('a * 'b) * 'rest) t -> (('a, 'b) Memory_proto_alpha.Script_typed_ir.pair * 'rest, 'b * 'rest) descr
  *)

end

module Option_ops : sig

  val cons : ('a * 'b) t -> ('a * 'b, 'a option * 'b) descr

  (*
  val cond : 
      ?target:'b t ->
      ('r t -> ('r, 'b) descr) ->
      (('a * 'r) t -> ('a * 'r, 'b) descr) ->
      ('a option * 'r, 'b) code
  val force_some : ?msg:string -> ('a option * 'r, 'a * 'r) code
  *)

end

module Union_ops : sig
  open Script_typed_ir

  val left : 'b ty -> ('a * 'r, ('a, 'b) union * 'r) code
  val right : 'a ty -> ('b * 'r, ('a, 'b) union * 'r) code 
  val loop : ?after:('b * 'r) t -> ('a * 'r, ('a, 'b) union * 'r) code -> (('a, 'b) union * 'r, 'b * 'r) code

end

module Arithmetic : sig

  val neg : (int_num * 'r, int_num *'r) code
  val abs : (int_num * 'r, nat *'r) code
  val nat_neq : (nat * 'a) t -> (nat * 'a, bool * 'a) descr
  val add_intint : (int_num * (int_num * 'rest)) t ->
      (int_num * (int_num * 'rest), int_num * 'rest) descr

  val mul_natnat : (nat * (nat * 'rest)) t -> (nat * (nat * 'rest), nat * 'rest) descr
  val mul_intint : (int_num * (int_num * 'rest)) t ->
      (int_num * (int_num * 'rest), int_num * 'rest) descr
  val force_ediv_tez : 
      (AC.Tez.tez * ( nat * 'a )) t ->
      (AC.Tez.tez * ( nat * 'a ),
      (AC.Tez.tez, AC.Tez.tez) Memory_proto_alpha.Script_typed_ir.pair * 'a) descr
  val div_n :  int -> (nat * 'a) t -> (nat * 'a, nat * 'a) descr
  val add_n : int -> (nat * 'a) t -> (nat * 'a, nat * 'a) descr
  val add_teztez_n : int -> (AC.Tez.tez * 'a) t -> (AC.Tez.tez * 'a, AC.Tez.tez * 'a) descr
  val force_nat : (int_num * 'a) t -> (int_num * 'a, nat * 'a) descr

  (*
  val neq : (int_num * 'r, bool *'r) code
  val int : (nat * 'r, int_num*'r) code
  val nat_opt : (int_num * 'r, nat option * 'r) code
  val add_natnat : (nat * (nat * 'rest)) t -> (nat * (nat * 'rest), nat * 'rest) descr
  val add_teztez : (AC.Tez.tez * (AC.Tez.tez * 'rest), AC.Tez.tez * 'rest ) code
  val sub_intint : (int_num * (int_num * 'r), int_num * 'r) code
  val sub_natnat : (nat * (nat * 'r), int_num * 'r) code
  val ediv : (nat * (nat * 'r), (nat * nat) option * 'r) code
  val ediv_tez :
      (AC.Tez.tez * ( nat * 'a )) t ->
      (AC.Tez.tez * ( nat * 'a ),
      (AC.Tez.tez, AC.Tez.tez) Memory_proto_alpha.Script_typed_ir.pair
      option * 'a) descr

  val force_ediv : (nat * (nat * 'a)) t -> (nat * (nat * 'a), (nat * nat) * 'a) descr
  val div : (nat * (nat * 'a)) t -> (nat * (nat * 'a), nat * 'a) descr
  val sub_n : int -> (nat * 'a) t -> (nat * 'a, int_num * 'a) descr
  *)

end

module Boolean : sig 

  val bool_and : (bool * (bool * 'r), bool * 'r) code
  val bool_or : (bool * (bool * 'r), bool * 'r) code
  val loop : ('s, bool * 's) code -> ((bool * 's), 's) code

  (*
  val cond : ?target:'s t -> ('r t -> ('r, 's) descr) -> ('r t -> ('r, 's) descr) -> (bool * 'r, 's) code
  *)

end

module Comparison_ops : sig

  val cmp_bytes : (bytes * (bytes * 'a)) t -> (bytes * (bytes * 'a), int_num * 'a) descr
  val eq_n : int -> (nat * 'a) t -> (nat * 'a, bool * 'a) descr
  val lt : (int_num * 'r, bool * 'r) code
  val assert_positive_nat : (nat * 'a) t -> (nat * 'a, nat * 'a) descr
  val assert_cmp_ge_nat : (nat * (nat * 'r), 'r) code
  val assert_cmp_ge_timestamp : (AC.Script_timestamp.t * (AC.Script_timestamp.t * 'r), 'r) code

  (*
  val cmp : 
      'a Memory_proto_alpha.Script_typed_ir.comparable_ty ->
      ('a * ('a * 'b), int_num * 'b) code

  val eq : (int_num * 'r, bool * 'r) code
  val ge : (int_num * 'r, bool * 'r) code
  val gt : (int_num * 'r, bool * 'r) code
  val gt_nat : (nat * 'a) t -> (nat * 'a, bool * 'a) descr
  val cmp_ge_nat : (nat * (nat * 'r), bool * 'r) code
  val cmp_ge_timestamp : (AC.Script_timestamp.t * (AC.Script_timestamp.t * 'r), bool * 'r) code
  *)

end


module Bytes : sig

  val concat : (MBytes.t * (MBytes.t * 'rest), MBytes.t * 'rest) code
  val sha256 : (MBytes.t * 'rest, MBytes.t * 'rest) code
  val blake2b : (MBytes.t * 'rest, MBytes.t * 'rest) code

  (*
  open Script_typed_ir

  val pack : 'a ty -> ('a * 'r, bytes * 'r) code
  val unpack_opt : 'a ty -> (bytes * 'r, 'a option * 'r) code
  val unpack : 'a ty -> (bytes * 'b) t -> (bytes * 'b, 'a * 'b) descr
  *)

end


module Map : sig
  open Script_typed_ir

  type ('a, 'b) t = ('a, 'b) map

  val empty : 'a comparable_ty -> ('a, 'b) t
  val set : ( 'a , 'b ) t -> 'a -> 'b -> ( 'a , 'b ) t

  module Ops : sig
    val update : 
      ('a * ('b option * (('a, 'b) t * ('rest)))) Stack.t -> 
      ('a * ('b option * (('a, 'b) t * 'rest)), ('a, 'b) t * 'rest) descr

    val get : ?a:('a ty) -> 'b ty -> ('a * (('a, 'b) map * 'r), 'b option * 'r) code

    val big_get : 'a ty -> 'b ty -> ('a * (('a, 'b) big_map * 'r), 'b option * 'r) code

    val big_update : 
      ('a * ('b option * (('a, 'b) big_map * 'r)), ('a, 'b) big_map * 'r) code
  end
end

module List_ops : sig
  val nil : 'a ty -> 'b t -> ('b, 'a list * 'b) descr

  val cons : 
    ('a * ('a list * 'b)) t -> ('a * ('a list * 'b), 'a list * 'b) descr

  val cond : target:'a t ->
    (('b * ('b list * 'c)) t ->
    ('b * ('b list * 'c), 'a) descr) ->
    ('c t -> ('c, 'a) descr) ->
    ('b list * 'c) t -> ('b list * 'c, 'a) descr

  val list_iter : ('a * 'r, 'r) code -> ('a list * 'r, 'r) code
end

module Tez : sig

  val tez_nat : (AC.Tez.tez * 'a) t -> (AC.Tez.tez * 'a, nat * 'a) descr
  val amount_nat : 'a t -> ( 'a , nat * 'a ) descr
  
  (*
  val amount : ('r, AC.Tez.t * 'r) code
  *)

end

module Misc : sig

  val min_nat : (nat * (nat * 'r), nat * 'r) code 
  val debug_msg : string -> 'a t -> ( 'a, 'a ) descr
  val now : ('r, AC.Script_timestamp.t * 'r) code

  (*
  val debug : msg:string -> unit -> 'a t -> ( 'a , 'a ) descr
  *)

end
