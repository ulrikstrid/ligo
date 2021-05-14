open Trace
open Test_helpers
open Main_errors


module Environment = Environment
module Inferred = Ast_core
module Inference = Inference
module Simplified = Ast_core

let int () : (unit, _) result =
  let open Ast_core.Combinators in
  let pre = e_int (Z.of_int 32) in
  let open Inference in
  let e = Inferred.Environment.empty in
  let state = Solver.initial_state in
  let* (_, _post,t,new_state) = trace inference_tracer @@ type_expression_subst e state pre in
  let () = Solver.discard_state new_state in
  let open! Inferred in
  let open Combinators in
  let* () = trace_option (test_internal __LOC__) @@ assert_type_expression_eq (t, t_int ()) in
  ok ()

let init_env = Option.unopt_exn @@ Trace.to_option @@ Checking.decompile_env @@ 
  Environment.default Environment.Protocols.current

open Ast_core

module TestExpressions = struct
  let test_expression ?(env = init_env)
                      ?(state = Inference.Solver.initial_state)
                      (expr : expression)
                      (test_expected_ty : Inferred.type_expression) =
    let pre = expr in
    let open Inference in
    let open! Inferred in
    let* (_ , _post ,t,new_state) = trace inference_tracer @@ type_expression_subst env state pre in
    let () = Solver.discard_state new_state in
    Format.printf "Test, t = %a and t_init = %a" Inferred.PP.type_expression t Inferred.PP.type_expression @@ test_expected_ty;
    let* () = trace_option (test_internal __LOC__) @@ assert_type_expression_eq (t, test_expected_ty) in
    ok ()

  module I = Simplified.Combinators
  module O = Inferred.Combinators
  module E = Inferred.Environment

  let unit   () : (unit, _) result = test_expression I.(e_unit ())    O.(t_unit ())
  let int    () : (unit, _) result = test_expression I.(e_int (Z.of_int 32))     O.(t_int ())
  let bool   () : (unit, _) result = test_expression I.(e_bool true)  O.(t_bool ())
  let string () : (unit, _) result = test_expression I.(e_string (Standard "s")) O.(t_string ())
  let bytes  () : (unit, _) result =
    let b = I.e_bytes_hex "0b" in
    test_expression b  O.(t_bytes ())

  let option () : (unit,_) result = test_expression I.(e_some @@ e_int Z.zero) O.(t_option @@ t_int ())
  let bytes_pack () : (unit,_) result = test_expression I.(e_constant C_BYTES_PACK [e_string @@ Standard "pack"]) O.(t_bytes ())
  let bytes_unpack () : (unit,_) result = test_expression I.(e_ascription (e_constant C_BYTES_UNPACK [e_bytes_string @@ "unpack"]) (t_option @@ t_bytes ())) O.(t_option @@ t_bytes ())

  let add () : (unit,_) result = test_expression I.(e_constant C_ADD [e_int Z.zero; e_int Z.one]) O.(t_int ())
  let eq () : (unit,_) result = test_expression I.(e_constant C_EQ [e_int Z.zero; e_int Z.zero]) O.(t_bool ())
  let key_hash () : (unit,_) result = test_expression I.(e_constant C_HASH_KEY [e_key "toto"]) O.(t_key_hash ())
  let failwith () : (unit,_) result = test_expression I.(e_ascription (e_constant C_FAILWITH [e_int Z.zero]) (t_int ())) O.(t_int ())
  let application () : (unit, _) result =
    test_expression
      I.(e_application (e_lambda_ez (Location.wrap @@ Var.of_name "x") ~ascr:(t_int ()) (Some (t_int ())) (e_var "x")) @@ e_int Z.one)
      O.(t_int ())

  let lambda () : (unit, _) result =
    test_expression
      I.(e_lambda_ez (Location.wrap @@ Var.of_name "x") ~ascr:(t_int ()) (Some (t_int ())) (e_var "x"))
      O.(t_function (t_int ()) (t_int ()))

  let recursive () : (unit,_) result =
    let fun_name = Location.wrap @@ Var.of_name "sum" in
    let var      = Location.wrap @@ Var.of_name "n" in
    let lambda = I.{binder={var;ascr=Some(t_nat ())};
                    output_type = Some (t_nat ());
                    result=e_application (e_variable fun_name) (e_variable var)
                   } in
    test_expression
      I.(e_recursive fun_name (I.t_function (I.t_nat ()) (I.t_nat ())) lambda)
      O.(t_function (t_nat ()) (t_nat ()))

  let let_in () : (unit, _) result =
    test_expression 
      I.(e_let_in_ez (Location.wrap @@ Var.of_name "x") false (e_int Z.zero) @@ e_var "x")
      O.(t_int ())

  let let_in_ascr () : (unit, _) result =
    test_expression 
      I.(e_let_in_ez (Location.wrap @@ Var.of_name "x") ~ascr:(t_int ()) false (e_int Z.zero) @@ e_var "x")
      O.(t_int ())

  let constructor () : (unit, _) result =
    let variant_foo_bar = Inferred.t_sum_ez [
        ("Foo", Inferred.t_int () );
        ("Bar", Inferred.t_string () ); ]
    in
    test_expression
      ~env:(E.add_type (Var.of_name "test_t") variant_foo_bar E.empty)
      I.(e_constructor (Label "Foo") (e_int (Z.of_int 32)))
      variant_foo_bar

  let matching () : (unit, _) result =
    let variant_foo_bar = Inferred.t_sum_ez [
        ("Foo", Inferred.t_int () );
        ("Bar", Inferred.t_string () ); 
        ("Baz", Inferred.t_unit ());
        ("Qux", Inferred.t_list (t_int ()));
        ("Quux", Inferred.t_unit ());
      ]
    in
    let binder_wild : type_expression binder = {var=Location.wrap (Var.of_name "_");ascr=None} in
    let binder_x : type_expression binder = {var=Location.wrap (Var.of_name "x");ascr=None} in
    let binder_y : type_expression binder = {var=Location.wrap (Var.of_name "y");ascr=None} in
    test_expression
      ~env:(E.add_type (Var.of_name "test_t") variant_foo_bar E.empty)
      I.(e_matching (e_constructor (Label "Foo") (e_int (Z.of_int 32)))
        [
          {pattern = Location.wrap @@ P_variant (Label "Foo", Location.wrap @@ P_var binder_x); body = e_var "x"};
          {pattern= Location.wrap @@ P_variant (Label "Bar", Location.wrap @@ P_var binder_y); body=e_int Z.zero};
          {pattern= Location.wrap @@ P_variant (Label "Baz", Location.wrap @@ P_unit); body=e_int Z.zero};
          {pattern= Location.wrap @@ P_var binder_wild; body=e_int Z.zero}
        ]
      ) O.(t_int ())

  let record () : (unit, _) result =
    test_expression
      I.(e_record @@ LMap.of_list [(Label "foo", e_int (Z.of_int 32)); (Label "bar", e_string (Standard "foo"))])
      O.(make_t_ez_record [("foo", t_int ()); ("bar", t_string ())])

  let record_accessor () : (unit, _) result =
    test_expression
      I.(e_record_accessor (e_record @@ LMap.of_list [(Label "foo", e_int Z.zero)]) @@ Label "foo")
      O.(t_int ())

  let record_update () : (unit, _) result =
    test_expression
      I.(e_record_update (e_record @@ LMap.of_list [(Label "foo", e_int Z.zero); (Label "bar", e_string (Standard "foo"))]) (Label "foo") @@ e_int Z.one)
      O.(make_t_ez_record [("foo", t_int ()); ("bar", t_string ())])

  let tuple () : (unit, _) result =
    test_expression
      I.(e_record @@ LMap.of_list [(Label "0",e_int (Z.of_int 32)); (Label "1", e_string (Standard "foo"))])
      O.(make_t_ez_record [("0",t_int ()); ("1",t_string ())])
  
  let ascription () : (unit, _) result =
    test_expression
      I.(e_ascription (e_int Z.one) (t_int ()))
      O.(t_int ())
end

(* TODO: deep types (e.g. record of record)
   TODO: negative tests (expected type error) *)

let main = test_suite "Typer (from core AST)"
  [
    test "int" int ;
    test "unit"            TestExpressions.unit ;
    test "int2"            TestExpressions.int ;
    test "bool"            TestExpressions.bool ;
    test "string"          TestExpressions.string ;
    test "bytes"           TestExpressions.bytes ;    
    test "option"          TestExpressions.option ;    
    test "bytes_pack"      TestExpressions.bytes_pack ;    
    test "bytes_unpack"    TestExpressions.bytes_unpack ;    
    test "add"             TestExpressions.add ;
    test "eq"              TestExpressions.eq ;
    test "keyhash"         TestExpressions.key_hash ;
    test "failwith"        TestExpressions.failwith ;
    test "application"     TestExpressions.application ;
    test "lambda"          TestExpressions.lambda ;
    test "recursive"       TestExpressions.recursive ;
    test "let_in"          TestExpressions.let_in ;
    test "let_in_ascr"     TestExpressions.let_in_ascr ;
    test "constructor"     TestExpressions.constructor ;
    test "matching"        TestExpressions.matching ;
    test "record"          TestExpressions.record ;
    test "record_accessor" TestExpressions.record_accessor ;
    test "record_update"   TestExpressions.record_update ;
    test "tuple"           TestExpressions.tuple ;
    test "ascription"      TestExpressions.ascription ;
  ]
