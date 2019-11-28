open Trace

let source_to_typed syntax source_file =
  let%bind simplified  = Of_source.just_compile     source_file syntax in
  let%bind typed,state = Of_simplified.just_compile simplified         in
  let env = Ast_typed.program_environment typed in
  ok (typed,state,env)