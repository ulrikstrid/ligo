(* TODO: move this with the AST, probably? *)
module Solver_should_be_generated = Solver_should_be_generated
module Axioms = Axioms

module Type_variable = struct type t = Ast_typed.Types.type_variable end

module Opaque_type_variable = struct
  module Types = Ast_typed.Types
  module Compare = struct
    include Ast_typed.Compare
    module Solver_should_be_generated = Solver_should_be_generated
  end
  module PP = struct
    include Ast_typed.PP
    module Solver_should_be_generated = Solver_should_be_generated
  end
  module Yojson = Ast_typed.Yojson
  module Var = Var
  module Solver_types = Solver_types
  module Misc = Ast_typed.Misc
  module Reasons = Ast_typed.Reasons
  module Axioms = Axioms
end
module Check : Type_variable_abstraction.TYPE_VARIABLE_ABSTRACTION(Type_variable).S = Opaque_type_variable
