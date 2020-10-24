open Ast

(* This plug-in system ensures the following:

 * the state of the plug-ins can only store unification vars in a few
   specific types (ReprMap.t and ReprSet.t)

 * when two unification variables are aliased, the plug-in is forced
   to update its state accordingly (it can either update each map/set
   with the supplied function or discard the entire map/set, but it
   cannot forget to update one of the maps/sets in its state; it is
   technically possible to modify the maps/sets (e.g. take a random
   value from one map/set and add it to another) but this would hardly
   be done accidentally).

 * the ReprMap and ReprSet modules only allow monotonic updates
   (additions but no deletions), unless one has access to the
   comparison function (which we do not provide to other modules), and
   the 'typeVariable type is always quantified/hidden in positions
   where it could be used to remove from a map/set or completely empty
   it. *)

(* This is (temporary?) in a functor to give access to
   Typer_errors.typer_error. Maybe this file should be moved to
   src/passes to solve that issue and hardcode the type for errors
   (which would limit extensibility by plugins though). *)
module Dep_cycle (Typer_errors : sig type typer_error end) = struct
  open UnionFind
  (* The types are given in an approximative haskell, because its
     syntax is more readable and more consitent than OCaml's constant
     switch between module types and types *)

  (* merge_keys contains functions which merge the given keys of a map, or elements of a set *)
  (* data merge_keys old new = updater {
       map :: forall v . old → old → map old v → map new v
       set :: old → old → set old → set new
     }
  *)
  type ('old, 'new_) merge_keys = {
    map : 'v . ('old, 'v) ReprMap.t -> ('new_, 'v) ReprMap.t;
    set :       'old      ReprSet.t ->  'new_      ReprSet.t;
  }

  (* Each normalizer returns an updated database (after storing the
     incoming constraint) and a list of constraints, used when the
     normalizer rewrites the constraints e.g. into simpler ones. *)
  (* type normalizer state a b = state → a → (state, set b) *)
  type ('state, 'a , 'b) normalizer = 'state -> 'a -> ('state * 'b PolySet.t)

  (* type normalizer_rm state a = state → a → MonadError typer_error state *)
  type ('state, 'a) normalizer_rm = 'state -> 'a -> ('state, Typer_errors.typer_error) Trace.result

  (* t is the type of the state of the plugin
     data Plugin (t :: 🞰→🞰) = Plugin {
       create_state :: forall typeVariable . (typeVariable → typeVariable → int) → t typeVariable
       add_constraint :: normalizer (t typeVariable) type_constraint_simpl type_constraint_simpl
       remove_constraint :: normalizer_rm (t typeVariable) type_constraint_simpl
       merge_aliases :: forall old new . merge_keys old new → old → old → t old → t new
     }
  *)
  module type Plugin = sig
    type 'typeVariable t
    val create_state : cmp:('typeVariable -> 'typeVariable -> int) -> 'typeVariable t
    val add_constraint : ('type_variable t, type_constraint_simpl, type_constraint_simpl) normalizer
    val remove_constraint : ('type_variable t, type_constraint_simpl) normalizer_rm
    val merge_aliases : ('old, 'new_) merge_keys -> 'old t -> 'new_ t
  end

  (* The kind PerPluginType describes type-level functions which take
     a type Plugin.t, and produce an arbitrary type which can depend
     on it.

     e.g. given a module
       Ppt : PerPluginType
     the type
       Ppt.M(SomePlugin).t
     could be one of:
       type_variable SomePlugin.t
       int
       …
  *)
  (* type PerPluginType = 🞰→(🞰→🞰)→🞰 *)
  module type PerPluginType = functor (Plugin : Plugin) -> sig
    type t
  end

  (* These are two useful PerPlugin type-level functions. The first
     gives a `unit' type for each plugin, the second *)
  module PerPluginUnit = functor (Plugin : Plugin) -> struct type t = unit end
  module PerPluginState = functor (Plugin : Plugin) -> struct type t = type_variable Plugin.t end

  (* type MappedFunction (t :: 🞰) (Plugin :: 🞰→🞰) =
       Plugin t → MakeInType t → MakeOutType t *)
  module type MappedFunction = sig
    type extra_args
    module MakeInType : PerPluginType
    module MakeOutType : PerPluginType
    module F(Plugin : Plugin) : sig
      val f : extra_args -> MakeInType(Plugin).t -> MakeOutType(Plugin).t
    end
  end

  module type Plugins = sig
    (* S is a record-like module containing one field per plug-in *)
    module PluginFields : functor (Ppt : PerPluginType) -> sig module type S end

    (* A default value where the field for each plug-in has type unit *)
    module PluginFieldsUnit : PluginFields(PerPluginUnit).S

    (* A function which applies F to each field *)
    module MapPlugins : functor (F : MappedFunction) ->
    sig
      val f :
        F.extra_args ->
        (module PluginFields(F.MakeInType).S) ->
        (module PluginFields(F.MakeOutType).S)
    end
  end

  module MakeSolver(Plugins : Plugins) : sig
    module type PluginStates = Plugins.PluginFields(PerPluginState).S
    val main : unit -> (module PluginStates)
  end = struct
    module type PluginStates = Plugins.PluginFields(PerPluginState).S
    module type PluginUnits = Plugins.PluginFields(PerPluginUnit).S

    type pluginStates = (module PluginStates)
    let pluginFieldsUnit = (module Plugins.PluginFieldsUnit : PluginUnits)

    (* Function which merges all aliases withing a single plugin's state *)
    module MergeAliases = struct
      type extra_args = { other_repr : type_variable ; new_repr : type_variable }
      module MakeInType = PerPluginState
      module MakeOutType = PerPluginState
      module F(Plugin : Plugin) = struct
        let f { other_repr ; new_repr } state =
          let merge_keys = {
            map = (fun m -> ReprMap.alias ~other_repr ~new_repr m);
            set = (fun s -> (*ReprSet.alias a b s*) s);
          }
          in Plugin.merge_aliases merge_keys state
      end
    end

    (* Function which creates a plugin's initial state *)
    module CreateState = struct
      type extra_args = unit
      module MakeInType = PerPluginUnit
      module MakeOutType = PerPluginState
      module F(Plugin : Plugin) = struct
        let f () (() as _state) = Plugin.create_state ~cmp:Compare.type_variable
      end
    end

    (* WIP prototype of the solver's main loop with plugins *)
    let main () : pluginStates =
      (* Create the initial state for each plugin *)
      let module MapCreateState = Plugins.MapPlugins(CreateState) in
      let states = MapCreateState.f () pluginFieldsUnit in
      let (other_repr, new_repr) = failwith "should be returned by the union-find when aliasing two unification variables" in
      let module MapMergeAliases = Plugins.MapPlugins(MergeAliases) in
      let states = MapMergeAliases.f { other_repr ; new_repr } states in
      states
  end
end
