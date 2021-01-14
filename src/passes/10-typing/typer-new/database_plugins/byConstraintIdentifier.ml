open Ast_typed.Types
open Trace

type 'typeVariable t = (constraint_identifier, c_typeclass_simpl) PolyMap.t
let create_state ~cmp:_ =
  PolyMap.create ~cmp:Ast_typed.Compare.constraint_identifier

let add_constraint _repr state new_constraint =
  match new_constraint with
  | SC_Typeclass c -> PolyMap.add c.id_typeclass_simpl c state
  | _ -> state
let remove_constraint _repr state constraint_to_rm =
  match constraint_to_rm with
  | Ast_typed.Types.SC_Typeclass { id_typeclass_simpl; _ } ->
    (* TODO: a proper error instead of an exception *)
    ok @@ PolyMap.remove id_typeclass_simpl state
  | _ -> ok state

let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun _merge_keys state -> state

let pp _type_variable ppf state =
  let open PP_helpers in
  list_sep_d (pair Ast_typed.PP.constraint_identifier Ast_typed.PP.c_typeclass_simpl) ppf (PolyMap.bindings state)

let name = "by_constraint_identifier"

let find_opt : constraint_identifier -> 'type_variable t -> c_typeclass_simpl option = PolyMap.find_opt

let get_state_for_tests state = state
