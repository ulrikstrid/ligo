open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/gitlab_111.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "gitlab_111.religo", line 2, characters 0-3 at "let", after "=":
          This is an incorrect let binding. 
          -
          Examples of correct let bindings: 
          let a: int = 4;
          let (a: int, b: int) = (1, 2);
          let func = (a: int, b: int) => a + b;
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/missing_rpar.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "missing_rpar.religo", line 5, characters 0-3 at "let", after "m":
          Missing `)`.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/ident_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "ident_wild.religo", line 1, characters 10-11 at "2", after "z":
          This is not a correct expression.
          If you were trying to call a function, you need to change this code to use parentheses like this: 
            `z(2)`. 
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/let_vbar.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "let_vbar.religo", line 2, characters 8-11 at "222", after "let":
          This is an incorrect let binding. 
          -
          Examples of correct let bindings: 
          let a: int = 4;
          let (a: int, b: int) = (1, 2);
          let func = (a: int, b: int) => a + b;
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/switch_contr_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "switch_contr_wild.religo", line 2, characters 15-18 at "bar", after "Foo":
          Expected a `.` as part of the module path.          
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_constr_dot_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_constr_dot_wild.religo", line 1, characters 13-14 at "*", after ".":
          Expected an identifier as part of the module path. A correct identifier matches `[a-z][a-zA-Z0-9_]*`.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_constr_lpar_ident_rpar_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_constr_lpar_ident_rpar_wild.religo", line 1, characters 19-20 at "*", after ")":
          Expected either:
          - another variant case, like: `| Foo`
          - another declaration
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
    run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_constr_lpar_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_constr_lpar_wild.religo", line 1, characters 14-15 at "*", after "(":
          Not a valid type.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
    run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_const_rpar.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_const_rpar.religo", line 1, characters 13-14 at ")", after "Foo":
          Unexpected token. Expected either:
          - type arguments like: `(string, int)`
          - another variant case like: `| Foo(int)`
          - a declaration like: `type foo = Bar;` or `let foo = 1;`
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_constr_semi_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_constr_semi_wild.religo", line 1, characters 16-17 at "*", after ";":
          Expected a declaration.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_constr_vbar_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_constr_vbar_wild.religo", line 1, characters 15-18 at "let", after "|":
          Expected a constructor here. A correct constructor name matches `[A-Z][a-zA-Z0-9_]*`.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_constr_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_constr_wild.religo", line 1, characters 15-16 at "*", after "Foo":
          Expected a `.` as part of the module path.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_ident_arrow_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_ident_arrow_wild.religo", line 1, characters 18-19 at "*", after "=>":
          Not a valid type.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_ident_lpar_ident_type.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_ident_lpar_ident_type.religo", line 1, characters 20-24 at "type", after "foo":
          Expected either:
          - closing parentheses, like `)`
          - another argument, like `, foo)`
          - type argument(s), like `(string, int))`
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_ident_lpar_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_ident_lpar_wild.religo", line 1, characters 15-16 at "*", after "(":
          Not a valid type argument.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_ident_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_ident_wild.religo", line 1, characters 15-16 at "*", after "foo":
          Expected either:
          - another argument, like `, foo)`
          - a closing parentheses, like `)`
          - type argument(s), like `(string, int))`
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_ident_colon_constr_type.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_ident_colon_constr_type.religo", line 1, characters 18-22 at "type", after "Boo":
          Expected either:
          - type arguments, like: `(string, int)`
          - a comma and another record field declaration, like: `, foo: int`
          - a `}` to close the record
          - a `.` as part of the module path, like `z: Boo.some_identifier`
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_ident_colon_ident_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_ident_colon_ident_wild.religo", line 1, characters 21-22 at "*", after "bar":
          Expected either: 
          - type arguments, like: `(string, int)`
          - a comma and another record field declaration, like: `, foo: int` or `, bar` 
          - a `}` to close the record
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_ident_colon_lpar_ident_type.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_ident_colon_lpar_ident_type.religo", line 1, characters 22-26 at "type", after "foo":
          Expected either:
          - more function type arguments
          - a `)` to close the function type arguments
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_ident_colon_lpar_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_ident_colon_lpar_wild.religo", line 1, characters 19-20 at "*", after "(":
          Not a valid type.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;  
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_ident_colon_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_ident_colon_wild.religo", line 1, characters 19-20 at "*", after ":":
          Not a valid type.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;  
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_ident_comma_ident_colon_constr_type.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_ident_comma_ident_colon_constr_type.religo", line 1, characters 25-28 at "dog", after "Cat":
          Expected a `.` as part of the module path.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;  
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_ident_comma_ident_comma_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_ident_comma_ident_comma_wild.religo", line 1, characters 19-20 at "*", after ",":
          Not a valid type.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;  

    run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_ident_comma_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_ident_comma_wild.religo", line 1, characters 16-17 at "*", after ",":
          Not a valid type.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;  
    
      run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_ident_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_ident_wild.religo", line 1, characters 17-18 at "*", after "bar":
          Expected a `:`, `}` or `,`.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;  
      run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lbrace_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lbrace_wild.religo", line 1, characters 13-16 at "Boo", after "{":
          A record field name has to start with a lowercase character.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;   
      run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lpar_constr_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lpar_constr_wild.religo", line 1, characters 16-17 at "*", after "Boo":
          Expected a `.` as part of the module path.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;     
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lpar_ident_comma_ident_comma_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lpar_ident_comma_ident_comma_wild.religo", line 1, characters 22-23 at "*", after ",":
          Not a valid type.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;     
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lpar_ident_comma_ident_rpar_arrow_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lpar_ident_comma_ident_rpar_arrow_wild.religo", line 1, characters 21-22 at "<", after "=>":
          Not a valid type.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;     
run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_lpar_ident_comma_ident_rpar_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_lpar_ident_comma_ident_rpar_wild.religo", line 1, characters 18-19 at "*", after ")":
          Expected either:
          - another declaration
          - a `=>` as part of the function type
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;     
    


run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_vbar_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_vbar_wild.religo", line 1, characters 11-12 at "*", after "|":
          Expected a constructor here. Did you perhaps forget to capitalize it?
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;   

run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_eq_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_eq_wild.religo", line 1, characters 11-12 at "*", after "=":
          Not a valid type expression. A valid type expression can either be:
          - a type alias like: `type foo = string;`
          - a type with arguments: `type foo = big_map (string, int);`
          - a variant type like: `type foo = | Foo | Bar (int);`
          - a tuple type like: `type foo = (string, int, bool);`
          - a record type like: `type foo = { foo: int, bar: string, etc: bool };`
          - a function type like: `type foo = (int) => int;`
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;   

run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_ident_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_ident_wild.religo", line 1, characters 9-10 at "*", after "foo":
          Expected an `=` as part of the type declaration.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;   


run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/type_wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "type_wild.religo", line 1, characters 5-6 at "*", after "type":
          Expected an identifier as part of the type declaration. A correct identifier matches `[a-z][a-zA-Z0-9_]*`.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;   

 run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/error_messages/wild.religo" ; "main" ] ;
  [%expect {|
    ligo: : Parse error in file "wild.religo", line 1, characters 0-1, at "*":
          Unexpected token.
           {}


     If you're not sure how to fix this error, you can
     do one of the following:

    * Visit our documentation: https://ligolang.org/docs/intro/introduction
    * Ask a question on our Discord: https://discord.gg/9rhYaEt
    * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
    * Check the changelog by running 'ligo changelog' |} ] ;   