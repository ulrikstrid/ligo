(* selector / propagation rule for breaking down composite types
 * For now: break pair(a, b) = pair(c, d) into a = c, b = d *)

open Ast_typed.Misc
open Ast_typed.Types
open Typesystem.Solver_types
open Trace
open Typer_common.Errors
open Database_plugins.All_plugins
open Ast_typed.Reasons

type 'a flds = <
  grouped_by_variable : type_variable GroupedByVariable.t ;
  ..
> as 'a

type selector_output = output_break_ctor

let selector : type_constraint_simpl -> _ flds -> selector_output list =
  (* find two rules with the shape x = k(var …) and x = k'(var' …) *)
  fun type_constraint_simpl indexes ->
  match type_constraint_simpl with
  | SC_Constructor c ->
    (* finding other constraints related to the same type variable and
       with the same sort of constraint (constructor vs. constructor)
       is symmetric *)
    let other_cs = (GroupedByVariable.get_constraints_by_lhs c.tv indexes#grouped_by_variable).constructor in
    let cs_pairs = List.map (fun x -> { a_k_var = c ; a_k'_var' = x }) other_cs in
    cs_pairs
  | SC_Alias       _                -> [] (* TODO: ??? (beware: symmetry) *)
  | SC_Poly        _                -> [] (* TODO: ??? (beware: symmetry) *)
  | SC_Typeclass   _                -> []
  | SC_Row         _                -> []

(* when a = k(…) and b = k'(…) are in the db, aliasing a and b should
   check if they're non-empty (and in that case produce a
   selector_output for all pairs / more efficiently any single pair
   since the break_ctor creates equivalence classes for the
   constructor arguments) *)

let alias_selector : type_variable -> type_variable -> _ flds -> selector_output list =
  fun old new_ indexes ->
  let old_ctors = (GroupedByVariable.get_constraints_by_lhs old indexes#grouped_by_variable).constructor in
  let new_ctors = (GroupedByVariable.get_constraints_by_lhs new_ indexes#grouped_by_variable).constructor in
  match old_ctors with
    [] -> []
  | old_ctors_hd :: _ ->
    (match new_ctors with
       [] -> []
     | new_ctors_hd :: _ ->
       [{ a_k_var = old_ctors_hd ; a_k'_var' = new_ctors_hd }])

let propagator : (output_break_ctor, typer_error) propagator =
  fun selected ->
  let a = selected.a_k_var in
  let b = selected.a_k'_var' in

  (* The selector is expected to provice two constraints with the shape x = k(var …) and x = k'(var' …) *)
  assert (Var.equal (a : c_constructor_simpl).tv (b : c_constructor_simpl).tv);

  (* produce constraints: *)

  (* a.tv = b.tv *)
  let eq1 = c_equation (wrap (Propagator_break_ctor "a") @@ P_variable a.tv) (wrap (Propagator_break_ctor "b") @@ P_variable b.tv) "propagator: break_ctor" in
  let () = if Ast_typed.Debug.debug_new_typer then
      let p = Ast_typed.PP.c_constructor_simpl in
      Printf.fprintf stderr "%s" @@ Format.asprintf "\npropagator_break_ctor\na = %a\nb = %a\n%!" p a p b in
  (* a.c_tag = b.c_tag *)
  if (Solver_should_be_generated.compare_simple_c_constant a.c_tag b.c_tag) <> 0 then
    (* TODO : use error monad *)
    failwith (Format.asprintf "type error: incompatible types, not same ctor %a vs. %a (compare returns %d)"
                Solver_should_be_generated.debug_pp_c_constructor_simpl a
                Solver_should_be_generated.debug_pp_c_constructor_simpl b
                (Solver_should_be_generated.compare_simple_c_constant a.c_tag b.c_tag))
  else
    (* Produce constraint a.tv_list = b.tv_list *)
    let aux = fun aa bb -> c_equation (wrap (Propagator_break_ctor "a") @@ P_variable aa) (wrap (Propagator_break_ctor "a") @@ P_variable bb) "propagator: break_ctor" in
    let%bind eqs3 = List.map2 aux a.tv_list b.tv_list
        ~ok ~fail:(fun _ _ -> fail @@ different_constant_tag_number_of_arguments __LOC__ a.c_tag b.c_tag (List.length a.tv_list) (List.length b.tv_list)) in
    let eqs = eq1 :: eqs3 in
    ok [
      {
        remove_constraints = [];
        add_constraints = eqs;
        proof_trace = Axiom Axioms.f_equal
      }
    ]

let printer = Ast_typed.PP.output_break_ctor
let printer_json = Ast_typed.Yojson.output_break_ctor
let comparator = Solver_should_be_generated.compare_output_break_ctor

let heuristic = Heuristic_plugin { selector; alias_selector; propagator; printer; printer_json; comparator }