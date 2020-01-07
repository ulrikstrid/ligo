open Cli_expect

let%expect_test _ =
    run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.mligo" ; "main" ] ;
  [%expect {| 
../../test/lexer/broken_string.mligo:1:9: warning: missing terminating '"' character [-Winvalid-pp-token]
let a = "broken
        ^
../../test/lexer/broken_string.mligo:4:6: warning: missing terminating '"' character [-Winvalid-pp-token]
lines";
     ^
2 warnings generated.
ligo: lexer error: The string starting here is interrupted by a line break.
      Hint: Remove the break, close the string before or insert a backslash.
       {"parser_loc":"in file \"broken_string.mligo\", line 1, characters 8-9"}
      
 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.religo" ; "main" ] ;
  [%expect {| 
../../test/lexer/broken_string.religo:1:9: warning: missing terminating '"' character [-Winvalid-pp-token]
let a = "broken
        ^
../../test/lexer/broken_string.religo:4:6: warning: missing terminating '"' character [-Winvalid-pp-token]
lines";
     ^
2 warnings generated.
ligo: lexer error: The string starting here is interrupted by a line break.
      Hint: Remove the break, close the string before or insert a backslash.
       {"parser_loc":"in file \"broken_string.religo\", line 1, characters 8-9"}
      

 |} ];

run_ligo_bad [ "compile-contract" ; "../../test/lexer/broken_string.religo" ; "main" ] ;
  [%expect {| 
../../test/lexer/broken_string.religo:1:9: warning: missing terminating '"' character [-Winvalid-pp-token]
let a = "broken
        ^
../../test/lexer/broken_string.religo:4:6: warning: missing terminating '"' character [-Winvalid-pp-token]
lines";
     ^
2 warnings generated.
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

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.mligo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Unexpected character '\239'.
       {"parser_loc":"in file \"unexpected_character.mligo\", line 1, characters 8-9"}

 |} ];

 run_ligo_bad [ "compile-contract" ; "../../test/lexer/unexpected_character.religo" ; "main" ] ;
  [%expect {| 
ligo: lexer error: Unexpected character '\239'.
       {"parser_loc":"in file \"unexpected_character.religo\", line 1, characters 8-9"}
 |} ]