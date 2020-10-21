include Ast                     (* TODO: this is quick & dirty, should write the open statements we need here *)
    
(* This is in a functor to fix a dependency cycle between ast.ml and
   typer_errors.ml. *)
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
  type ('state, 'a) normalizer_rm = 'state -> 'a -> ('state, Typer_errors.typer_error) result

  (* t is the type of the state of the plugin
     data Plugin (t :: 🞰→🞰) = Plugin {
       empty :: forall typeVariable . (typeVariable → typeVariable → int) → t typeVariable
       add_constraint :: normalizer (t typeVariable) type_constraint_simpl type_constraint_simpl
       remove_constraint :: normalizer_rm (t typeVariable) type_constraint_simpl
       merge_aliases :: forall old new . merge_keys old new → old → old → t old → t new
     }
  *)
  module type Plugin = sig
    type 'typeVariable t
    val empty : cmp:('typeVariable -> 'typeVariable -> int) -> 'typeVariable t
    val add_constraint : (type_variable t, type_constraint_simpl, type_constraint_simpl) normalizer
    val remove_constraint : (type_variable t, type_constraint_simpl) normalizer_rm
    val merge_aliases : ('old, 'new_) merge_keys -> 'old t -> 'new_ t
  end

  (* Haskell doesn't have easy-to-use type-level functions or types as
     fields of records, so we're bending its syntax here.

     type "Assignments.t" typeVariable = map typeVariable
     c_constructor_simpl data Assignments :: Plugin "Assignments.t" *)
  module Assignments : Plugin = struct
    type 'typeVariable t = ('typeVariable, c_constructor_simpl) ReprMap.t
    let empty ~cmp =
      let merge c1 c2 = (* assert (Compare.c_constructor_simpl c1 c2 = 0); *) ignore c2; c1 in
      ReprMap.create ~cmp ~merge
    let add_constraint _ = failwith "todo"
    let remove_constraint _ = failwith "todo"
    let merge_aliases : 'old 'new_ . ('old, 'new_) merge_keys -> 'old t -> 'new_ t =
      fun merge_keys state -> merge_keys.map state
  end

  module GroupedByVariable : Plugin = struct
    (* map from (unionfind) variables to constraints containing them *)
    type 'typeVariable t = ('typeVariable, constraints) ReprMap.t
    let empty ~cmp =
      let merge cs1 cs2 = (* assert (Compare.constraints cs1 cs2 = 0); *) ignore cs2; cs1 in
      ReprMap.create ~cmp ~merge
    let add_constraint _ = failwith "todo"
    let remove_constraint _ = failwith "todo"
    let merge_aliases =
      fun updater state -> updater.map state
  end
  
  (* The kind PerPluginType describes type-level functions
     which take a type for 'typeVariable, a type Plugin.t, and
     produces an arbitrary type which can depend on these two.

     e.g. given a module
       Ppt : PerPluginType
     the type
       Ppt.M(type_variable)(SomePlugin).t
     could be one of:
       type_variable SomePlugin.t
       int SomePlugin.t
       type_variable list
       int
       …
  *)
  (* type PerPluginType = 🞰→(🞰→🞰)→🞰 *)
  module type PerPluginType = functor (TypeVariable : sig type t end)(Plugin : sig type 'typeVariable t end) -> sig
    type t
  end

  (* data PluginFields (TypeVariable :: 🞰) (Ppt :: PerPluginType) = PluginFields {
       assignments       :: Ppt TypeVariable Assignments,
       groupedByVariable :: Ppt TypeVariable GroupedByVariable,
       …
     }
  *)
  module PluginFields (TypeVariable : sig type t end) (Ppt : PerPluginType) = struct
    module type S = sig
      val assignments         : Ppt(TypeVariable)(Assignments).t
      val grouped_by_variable : Ppt(TypeVariable)(GroupedByVariable).t
    end
  end

  (* type Functor (t :: 🞰) (Plugin :: 🞰→🞰) (InTypeVariable :: 🞰) (OutTypeVariable :: 🞰) =
       Plugin t → MakeInType InTypeVariable t → MakeOutType OutTypeVariable t *)
  module type Functor = sig
    module MakeInType : PerPluginType
    module MakeOutType : PerPluginType
    module InTypeVariable : sig type t end
    module OutTypeVariable : sig type t end
    module F(Plugin : Plugin) : sig
      val f : MakeInType(InTypeVariable)(Plugin).t -> MakeOutType(OutTypeVariable)(Plugin).t
    end
  end

  (* mapPlugins :: (F : Functor) → (PluginFields F.MakeIn) → (PluginFields F.MakeOut) *)
  module MapPlugins(F : Functor) = struct
    let f :
      (module PluginFields(F.InTypeVariable)(F.MakeInType).S) ->
      (module PluginFields(F.OutTypeVariable)(F.MakeOutType).S)
      = fun fieldsIn ->
        let module FieldsIn = (val fieldsIn) in
        (module struct
          let assignments = (let module F = F.F(Assignments) in F.f FieldsIn.assignments)
          let grouped_by_variable = (let module F = F.F(GroupedByVariable) in F.f FieldsIn.grouped_by_variable)
        end)
  end

  module TypeVariable = struct type t = type_variable end
  
  module PerPluginState = functor (TypeVariable : sig type t end) (Plugin : sig type 'typeVariable t end) -> struct
    type t = TypeVariable.t Plugin.t
  end
  
  module type PluginStates = PluginFields(TypeVariable)(PerPluginState).S
  type pluginStates = (module PluginStates)

  (* An empty value for each plugin name *)
  module Unit = struct type t = unit end
  module PerPluginUnit = functor (TypeVariable : sig [@warning "-34"] type t end) (Plugin : sig [@warning "-34"] type 'typeVariable t end) -> struct
    type t = unit
  end
  module type PluginUnits = PluginFields(Unit)(PerPluginUnit).S
  module PluginFieldsUnit : PluginUnits = struct
    let assignments = ()
    let grouped_by_variable = ()
  end
  let pluginFieldsUnit = (module PluginFieldsUnit : PluginUnits)

  (* Function which merges all aliases withing a single plugin's state *)
  module MergeAliases = struct
    module MakeInType = PerPluginState
    module MakeOutType = PerPluginState
    module InTypeVariable = struct type t = type_variable end
    module OutTypeVariable = struct type t = type_variable end
    module F(Plugin : Plugin) = struct
      let f state =
        let other_repr = failwith "TODO: thread the other_repr until here" in
        let new_repr = failwith "TODO: thread the new_repr until here" in
        let merge_keys = {
          map = (fun m -> ReprMap.alias ~other_repr ~new_repr m);
          set = (fun s -> (*ReprSet.alias a b s*) s);
        } in
        Plugin.merge_aliases merge_keys state
    end
  end

  (* Function which creates a plugin's initial state *)
  module CreateState = struct
    module MakeInType = PerPluginUnit
    module MakeOutType = PerPluginState
    module InTypeVariable = struct type t = unit end
    module OutTypeVariable = struct type t = type_variable end
    module F(Plugin : Plugin) = struct
      let f (() as _state) = Plugin.empty ~cmp:Compare.type_variable
    end
  end

  (* WIP prototype of the solver's main loop with plugins *)
  let main (pluginStates : pluginStates) : pluginStates =
    let module MapCreateState = MapPlugins(CreateState) in
    let _ = MapCreateState.f pluginFieldsUnit in
    let module MapMergeAliases = MapPlugins(MergeAliases) in
    MapMergeAliases.f pluginStates
end
