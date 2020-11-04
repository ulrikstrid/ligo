module Node = struct
  type t = String.t
  let compare = String.compare
  let hash    = Hashtbl.hash
  let equal   = String.equal
end

module G=Graph.Persistent.Digraph.Concrete(Node)
module GP = Graph.Graphml.Print(G)
module Dfs = Graph.Traverse.Dfs(G)
module SMap = Map.String
module Errors = Errors

open Trace
open Errors

type file_name = string
type graph = G.t * (Compile.Helpers.meta * Compile.Of_core.form * Buffer.t * (string * string) list) SMap.t

let dependency_graph : options:Compiler_options.t -> string -> Compile.Of_core.form -> file_name -> (graph, _) result =
  fun ~options syntax form file_name ->
  let vertices = SMap.empty in
  let dep_g = G.empty in
  let rec dfs acc (dep_g,vertices) (file_name,form) =
    if not @@ SMap.mem file_name vertices then
      let%bind meta = trace compiler_error @@ Compile.Of_source.extract_meta syntax file_name in
      let%bind c_unit, deps = trace compiler_error @@ Compile.Of_source.compile ~options ~meta file_name in
      let vertices = SMap.add file_name (meta,form,c_unit,deps) vertices in
      let dep_g = G.add_vertex dep_g file_name in
      let dep_g =
        (* Don't add a loop on the first element *)
        if String.equal acc file_name then dep_g
        else G.add_edge dep_g acc file_name
      in
      let files = List.map (fun (a,_) -> (a,Compile.Of_core.Env)) deps in
      let%bind dep_g,vertices = bind_fold_list (dfs file_name) (dep_g,vertices) files in
      ok @@ (dep_g,vertices)
    else
      let dep_g = G.add_edge dep_g acc file_name in
      ok @@ (dep_g,vertices)
  in
  dfs file_name (dep_g,vertices) @@ (file_name,form)

let print_graph dep_g =
  Dfs.prefix (Format.printf "Node : %s\n%!") dep_g

let solve_graph : graph -> (_ list,_) result =
  fun (dep_g,vertices) ->
  if Dfs.has_cycle dep_g
  then (
    print_graph dep_g;
    fail @@ dependency_cycle ()
  )
  else
    let order = ref [] in
    let aux v =
      let elem = SMap.find v vertices in
      order := (v,elem)::!order
    in
    (* Their prefix order is broken, putting the root in the middle of the list
      TODO: use our own graph library
    *)
    Dfs.postfix aux dep_g;
    let order = List.rev @@ !order in
    ok @@ order

let add_module_in_env init_env _deps = init_env (*TODO*)

(* TODO *)
let build_michelson order_deps asts_typed entry_point =
  let rec last = function
    [] -> fail @@ corner_case ~loc:__LOC__ "at least a file is being processed"
  | hd :: [] -> ok @@ hd
  | _ :: tl -> last tl
  in
  let%bind typed,_ = (last order_deps) in
  let%bind typed =
    trace_option (corner_case ~loc:__LOC__ "Present by contruction" ) @@
    SMap.find_opt typed asts_typed
  in
  let%bind mini_c     = trace compiler_error @@ Compile.Of_typed.compile typed in
  let%bind michelson  = trace compiler_error @@ Compile.Of_mini_c.aggregate_and_compile_contract mini_c entry_point in
  ok michelson

let build_contract : options:Compiler_options.t -> string -> string -> _ -> file_name -> (_, _) result =
  fun ~options syntax entry_point protocol_version file_name ->
    let%bind deps = dependency_graph syntax ~options (Contract entry_point) file_name in
    let%bind order_deps = solve_graph deps in
    let aux asts_typed (file_name, (meta,form,c_unit,deps)) =
      let%bind ast_core = trace compiler_error @@ Compile.Utils.to_core ~options ~meta c_unit file_name in
      let aux (file_name,module_name) =
        let%bind ast_typed =
          trace_option (corner_case ~loc:__LOC__
          "File compiled before dependency. The build system is broken, contact the devs")
          @@ SMap.find_opt file_name asts_typed
        in
        ok @@ (module_name, ast_typed)
      in
      let%bind deps = bind_map_list aux deps in
      let%bind init_env   = trace compiler_error @@ Compile.Helpers.get_initial_env protocol_version in
      let init_env = add_module_in_env init_env deps in
      let%bind ast_typed,_,_ = trace compiler_error @@ Compile.Of_core.compile ~typer_switch:options.typer_switch ~init_env form ast_core in
      ok @@ SMap.add file_name ast_typed asts_typed
    in
    let%bind asts_typed = bind_fold_list aux (SMap.empty) order_deps in
    let%bind contract   = build_michelson order_deps asts_typed entry_point in
    ok contract
