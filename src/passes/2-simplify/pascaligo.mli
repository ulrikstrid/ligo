(** Converts PascaLIGO programs to the Simplified Abstract Syntax Tree. *)

open Trace
open Ast_simplified

module Raw = Parser.Pascaligo.AST
module SMap = Map.String

(** Convert a concrete PascaLIGO expression AST to the simplified
    expression AST used by the compiler. *)
val simpl_expression : loc_form:(Region.t -> Location.t) -> Raw.expr -> expr result

(** Convert a concrete PascaLIGO program AST to the simplified program
    AST used by the compiler. *)
val simpl_program : loc_form:(Region.t -> Location.t) -> Raw.ast -> program result
