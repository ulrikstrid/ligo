open Helpers
open Ast_imperative
open Monad
open QCheck

let binary_num_constants = [C_MUL; C_DIV; C_MOD; C_SUB; C_ADD]
let binary_bool_constants = [C_AND; C_OR; C_XOR]
let cmp_constants = [C_EQ; C_NEQ; C_LT; C_GT; C_LE; C_GE]

let op_class = [binary_num_constants; binary_bool_constants; cmp_constants]

let rec find_class op = function
  | [] -> raise Not_found
  | x :: _ when List.mem op x -> x
  | _ :: xs -> find_class op xs

let mutate_constant ({cons_name} as const) =
  let open Gen in
  match cons_name with
  | Const c when List.exists (List.mem c) op_class  ->
     let ops = find_class c op_class in
     let mapper x = return { const with cons_name = Const x } in
     oneof @@ List.map mapper ops
  | _ ->
     return const

let mutate_expression (expr : expression) =
  let open Gen in
  match expr.expression_content with
  | E_constant c ->
     let%bind c = mutate_constant c in
     return { expr with expression_content = E_constant c }
  | _ ->
     return expr

let mutate_module_ (mod_ : module_) =
  let rndmod_ = map_module (Expression mutate_expression) mod_ in
  Trace.ok @@ Gen.generate1 rndmod_
