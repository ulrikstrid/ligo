open Cli_expect

let%expect_test _ =

    run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: The string starting here is interrupted by a line break.
      Hint: Remove the break, close the string before or insert a backslash.
       {"parser_loc":"in file \"broken_string.mligo\", line 1, characters 8-9"}      
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: The string starting here is interrupted by a line break.
      Hint: Remove the break, close the string before or insert a backslash.
       {"parser_loc":"in file \"broken_string.religo\", line 1, characters 8-9"}
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/negative_byte_sequence.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Negative byte sequence.
      Hint: Remove the leading minus sign.
       {"parser_loc":"in file \"negative_byte_sequence.mligo\", line 1, characters 8-13"}
      
 |} ];


run_ligo_bad [ "compile-contract" ; "../../test/lexer/negative_byte_sequence.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Negative byte sequence.
      Hint: Remove the leading minus sign.
       {"parser_loc":"in file \"negative_byte_sequence.religo\", line 1, characters 8-13"}
            
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/reserved_name.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Reserved name: end.
      Hint: Change the name.
       {"parser_loc":"in file \"reserved_name.religo\", line 1, characters 4-7"}
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/reserved_name.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Reserved name: object.
      Hint: Change the name.
       {"parser_loc":"in file \"reserved_name.mligo\", line 1, characters 4-10"}
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Unexpected character '\239'.
       {"parser_loc":"in file \"unexpected_character.mligo\", line 1, characters 8-9"}

 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Unexpected character '\239'.
       {"parser_loc":"in file \"unexpected_character.religo\", line 1, characters 8-9"}
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unterminated_comment.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Unterminated comment.
      Hint: Close with "*)".
       {"parser_loc":"in file \"unterminated_comment.mligo\", line 1, characters 0-2"}
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_symbol.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid symbol.
      Hint: Check the LIGO syntax you use.
       {"parser_loc":"in file \"invalid_symbol.mligo\", line 1, characters 10-13"}
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_symbol.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid symbol.
      Hint: Check the LIGO syntax you use.
       {"parser_loc":"in file \"invalid_symbol.religo\", line 1, characters 10-11"}
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/missing_break.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Missing break.
      Hint: Insert some space.
       {"parser_loc":"in file \"missing_break.mligo\", line 1, characters 11-11"}
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/missing_break.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Missing break.
      Hint: Insert some space.
       {"parser_loc":"in file \"missing_break.religo\", line 1, characters 11-11"}
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_character_in_string.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid character in string.
      Hint: Remove or replace the character.
       {"parser_loc":"in file \"invalid_character_in_string.mligo\", line 1, characters 9-10"}
 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/invalid_character_in_string.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Invalid character in string.
      Hint: Remove or replace the character.
       {"parser_loc":"in file \"invalid_character_in_string.religo\", line 1, characters 9-10"}
 |} ]