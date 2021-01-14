open Trace
open Ast_typed.Types

type 'typeVariable t = unit
let create_state ~cmp:_ = ()
let add_constraint _repr () _constraint = ()
let remove_constraint _repr () _constraint = ok ()
let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
  fun _merge_keys state -> state

let pp _type_variable ppf () = Format.fprintf ppf "()"

let name = "cycle_detection_topological_sort"

let get_state_for_tests state = state
