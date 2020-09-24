open Cli_expect

let%expect_test _ =
  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/gitlab_111.religo" ; "main" ] ;
  [%expect {|
    ligo: error
          Parse error in file "gitlab_111.religo", line 2, characters 0-3 at "let", after "=":
          184: <syntax error>


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

  run_ligo_bad [ "compile-contract" ; "../../test/contracts/negative/missing_rpar.religo" ; "main" ] ;
  [%expect {|
    ligo: error
          Parse error in file "missing_rpar.religo", line 5, characters 0-3 at "let", after "m":
          273: <syntax error>


          If you're not sure how to fix this error, you can do one of the following:

          * Visit our documentation: https://ligolang.org/docs/intro/introduction
          * Ask a question on our Discord: https://discord.gg/9rhYaEt
          * Open a gitlab issue: https://gitlab.com/ligolang/ligo/issues/new
          * Check the changelog by running 'ligo changelog' |} ] ;

