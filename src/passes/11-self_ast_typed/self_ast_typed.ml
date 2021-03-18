open Trace
module Errors = Errors
module Helpers = Helpers

let all_module_passes = [
  Helpers.map_module Tail_recursion.peephole_expression ;
  Helpers.map_module Michelson_layout.peephole_expression ;
  Unused.unused_map_module ;
]

let all_expression_passes = [
  Helpers.map_expression Tail_recursion.peephole_expression ;
  Helpers.map_expression Michelson_layout.peephole_expression ;
]

let contract_passes = [
  Contract_passes.self_typing ;
  No_nested_big_map.self_typing ;
]

let all_module =
  bind_chain all_module_passes

let all_expression =
  bind_chain all_expression_passes

let all_contract main_name prg =
  let%bind contract_type = Helpers.fetch_contract_type main_name prg in
  let data : Contract_passes.contract_pass_data = {
    contract_type = contract_type ;
    main_name = main_name ;
    } in
  let all_p = List.map (fun pass -> Helpers.fold_map_module pass data) contract_passes in
  bind_chain_ignore_acc all_p prg
let all = [
  Tail_recursion.peephole_expression
]

let map_expression = Helpers.map_expression

let fold_expression = Helpers.fold_expression

let fold_map_expression = Helpers.fold_map_expression
