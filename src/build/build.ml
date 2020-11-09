module Node = struct
  type t = String.t
  let compare = String.compare
  let hash    = Hashtbl.hash
  let equal   = String.equal
end

module G = Graph.Persistent.Digraph.Concrete(Node)
(* module GP = Graph.Graphml.Print(G)(L) *)
module Dfs = Graph.Traverse.Dfs(G)
module SMap = Map.String
module SSet = Set.Make(String)
module Errors = Errors

open Trace
open Errors

type file_name = string
type graph = G.t * (Compile.Helpers.meta * Compile.Of_core.form * Buffer.t * (string * string) list) SMap.t

(* For printing : stolen from Christian's work *)
type state = <
  offsets  : bool;
  mode     : [`Point | `Byte];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

let mk_state ~offsets ~mode ~buffer =
  object
    method offsets  = offsets;
    method mode     = mode;
    method buffer   = buffer
    val pad_path    = ""
    method pad_path = pad_path
    val pad_node    = ""
    method pad_node = pad_node

    (* The method [pad] updates the current padding, which is
       comprised of two components: the padding to reach the new node
       (space before reaching a subtree, then a vertical bar for it)
       and the padding for the new node itself (Is it the last child
       of its parent?).

       A child node that is not the last satisfies [rank < arity] and
       the last child satisfies [rank = arity], where the rank of the
       first child is 0. *)

    method pad arity rank =
      {< pad_path =
           pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
         pad_node =
           pad_node ^ (if rank = arity-1 then "    " else "|   ")
      >}
  end

let print_graph state dep_g filename =
  let exception Dependency_cycle of string in
  let open Format in
  let len node =
    let aux _node i = i + 1 in
    G.fold_succ aux dep_g node 0
  in
  let set = SSet.empty in
  let rec pp_node state set arity name rank =
    let state = state#pad arity rank in
    let node = sprintf "%s%s\n%!" state#pad_path name in
    Buffer.add_string state#buffer node;
    if SSet.mem name set then raise (Dependency_cycle name);
    let set = SSet.add name set in
    let len = len name in
    let _ = G.fold_succ (pp_node state set len) dep_g name 0 in
    rank+1
  in
  let _ = try
    pp_node state set 1 filename 0
    with Dependency_cycle _ -> 0
  in ()


let to_json state dep_g filename =
  let set = SSet.empty in
  let rec pp_node _state set name parent =
    let node = ["file",  `String name] in
    if SSet.mem name set then ("child", `Assoc node)::parent
    else
      let set = SSet.add name set in
      let node = G.fold_succ (pp_node state set) dep_g name node in
      let node = List.rev node in
      ("child", `Assoc node)::parent
  in
  let root = ["root", `String filename] in
  let root =
    G.fold_succ (pp_node state set) dep_g filename root
  in `Assoc (List.rev root)


(* Build system *)

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

let solve_graph : graph -> file_name -> (_ list,_) result =
  fun (dep_g,vertices) file_name ->
  if Dfs.has_cycle dep_g
  then (
    let buffer = Buffer.create 59 in
    let state = mk_state
        ~offsets:true
        ~mode:`Point
        ~buffer in
    print_graph state dep_g file_name;
    fail @@ dependency_cycle @@ Buffer.contents state#buffer
  )
  else
    let aux v order =
      let elem = SMap.find v vertices in
      (v,elem)::order
    in
    let order = Dfs.fold_component aux [] dep_g file_name in
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
    let%bind order_deps = solve_graph deps file_name in
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

let pretty_print_graph =
  fun ~options syntax state source ->
  let%bind graph,_ = dependency_graph syntax ~options Env source in
  print_graph state graph source;
  ok @@ state#buffer
