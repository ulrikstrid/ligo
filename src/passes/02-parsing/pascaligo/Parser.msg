interactive_expr: Begin Case Verbatim Of LBRACKET WILD ARROW Skip With
##
## Ends in an error in state: 571.
##
## case(test_clause(instruction)) -> Case expr Of LBRACKET option(VBAR) cases(test_clause(instruction)) . RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Case expr Of LBRACKET option(VBAR) cases(test_clause(instruction))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 573, spurious reduction of production nsepseq(case_clause(test_clause(instruction)),VBAR) -> case_clause(test_clause(instruction))
## In state 570, spurious reduction of production cases(test_clause(instruction)) -> nsepseq(case_clause(test_clause(instruction)),VBAR)
##

Ill-formed case instruction.
At this point, if the case clause is complete, one of the following is
expected:
  * a vertical bar '|' followed by another clause;
  * a closing bracket ']' if the case is complete.
Note: A clause, here, is made of a pattern followed by an arrow '->'
and an instruction or a block.

interactive_expr: Begin Case Verbatim Of LBRACKET With
##
## Ends in an error in state: 540.
##
## case(test_clause(instruction)) -> Case expr Of LBRACKET option(VBAR) . cases(test_clause(instruction)) RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Case expr Of LBRACKET option(VBAR)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 539, spurious reduction of production option(VBAR) ->
##
interactive_expr: Begin Case Verbatim Of With
##
## Ends in an error in state: 576.
##
## case(test_clause(instruction)) -> Case expr Of option(VBAR) . cases(test_clause(instruction)) End [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Case expr Of option(VBAR)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 537, spurious reduction of production option(VBAR) ->
##
interactive_expr: Begin Case Verbatim Of WILD ARROW Skip VBAR With
##
## Ends in an error in state: 574.
##
## nsepseq(case_clause(test_clause(instruction)),VBAR) -> case_clause(test_clause(instruction)) VBAR . nsepseq(case_clause(test_clause(instruction)),VBAR) [ RBRACKET End ]
##
## The known suffix of the stack is as follows:
## case_clause(test_clause(instruction)) VBAR
##

Ill-formed case instruction.
At this point, a case clause is expected, made of a pattern followed
by an arrow '->' and an instruction or block.

interactive_expr: Begin Case Verbatim Of WILD ARROW Skip With
##
## Ends in an error in state: 577.
##
## case(test_clause(instruction)) -> Case expr Of option(VBAR) cases(test_clause(instruction)) . End [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Case expr Of option(VBAR) cases(test_clause(instruction))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 573, spurious reduction of production nsepseq(case_clause(test_clause(instruction)),VBAR) -> case_clause(test_clause(instruction))
## In state 570, spurious reduction of production cases(test_clause(instruction)) -> nsepseq(case_clause(test_clause(instruction)),VBAR)
##

Ill-formed case instruction.
At this point, if the clause is complete, one of the following is
expected:
  * a vertical bar '|' followed by another clause;
  * the keyword 'end' if the case is complete.
Note: A clause, here, is made of a pattern followed by an arrow '->'
and an instruction or a block.

interactive_expr: Begin Case Verbatim Of WILD ARROW With
##
## Ends in an error in state: 542.
##
## case_clause(test_clause(instruction)) -> pattern ARROW . test_clause(instruction) [ VBAR RBRACKET End ]
##
## The known suffix of the stack is as follows:
## pattern ARROW
##

Ill-formed clause of a case instruction.
At this point, one of the following is expected:
  * a single instruction;
  * a block of statements (instructions and declarations).
Note: A clause, here, is made of a pattern followed by an arrow '->'
and an instruction or a block.

interactive_expr: Begin Case Verbatim Of WILD With
##
## Ends in an error in state: 541.
##
## case_clause(test_clause(instruction)) -> pattern . ARROW test_clause(instruction) [ VBAR RBRACKET End ]
##
## The known suffix of the stack is as follows:
## pattern
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 225, spurious reduction of production pattern -> core_pattern
##

Ill-formed clause of a case instruction.
At this point, if the pattern is complete, an arrow '->' is expected,
followed by one of the following:
  * a single instruction;
  * a block of statements (instructions and declarations).
Note: A block is either delimited by '{' and '}', or 'begin' and
'end'.

interactive_expr: LBRACE Case Verbatim VBAR
##
## Ends in an error in state: 536.
##
## case(test_clause(instruction)) -> Case expr . Of LBRACKET option(VBAR) cases(test_clause(instruction)) RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
## case(test_clause(instruction)) -> Case expr . Of option(VBAR) cases(test_clause(instruction)) End [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Case expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

Ill-formed case instruction.
At this point, if the condition is complete, the keyword 'of' is
expected, followed with a series of case clauses separated by vertical
bars '|'.
Note: A clause, here, is made of a pattern followed by an arrow '->'
and an instruction or a block.

interactive_expr: Begin Case With
##
## Ends in an error in state: 535.
##
## case(test_clause(instruction)) -> Case . expr Of LBRACKET option(VBAR) cases(test_clause(instruction)) RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
## case(test_clause(instruction)) -> Case . expr Of option(VBAR) cases(test_clause(instruction)) End [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Case
##

Ill-formed case instruction.
At this point, the condition is expected as an expression, followed by
the keyword 'of'.

interactive_expr: Begin UIdent With
##
## Ends in an error in state: 359.
##
## module_path(field_path) -> UIdent . DOT module_path(field_path) [ LPAR LBRACKET ASSIGN ]
## module_path(field_path) -> UIdent . DOT field_path [ LPAR LBRACKET ASSIGN ]
## value_in_module -> UIdent . DOT Or [ LPAR LBRACKET ASSIGN ]
## value_in_module -> UIdent . DOT And [ LPAR LBRACKET ASSIGN ]
##
## The known suffix of the stack is as follows:
## UIdent
##

Ill-formed selection of a value from a module.
At this point, the selection operator '.' is expected, followed by the
qualified name of a value (that is, an access path to that value).

interactive_expr: Begin For Ident ARROW Ident In With
##
## Ends in an error in state: 531.
##
## for_in -> For Ident ARROW Ident In . Map expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ARROW Ident In
##

Ill-formed bounded loop over a map.
At this point, the keyword 'map' is expected, followed by a map
expression.

interactive_expr: Begin For Ident ARROW Ident In Map With
##
## Ends in an error in state: 532.
##
## for_in -> For Ident ARROW Ident In Map . expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ARROW Ident In Map
##

Ill-formed bounded loop over a map.
At this point, the map to be iterated is expected as an expression.

interactive_expr: LBRACE For Ident In Set Verbatim VBAR
##
## Ends in an error in state: 518.
##
## for_in -> For Ident In collection expr . block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident In collection expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

Ill-formed bounded loop over a map.
At this point, the body of the loop is expected as a block of
instructions.
Note: A block is either delimited by '{' and '}', or 'begin' and
'end'.

interactive_expr: Begin For Ident ARROW Ident With
##
## Ends in an error in state: 530.
##
## for_in -> For Ident ARROW Ident . In Map expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ARROW Ident
##

Ill-formed bounded loop over a map.
At this point, the keywords 'in map' are expected, followed by the map
to be iterated, as an expression.

interactive_expr: Begin For Ident ARROW With
##
## Ends in an error in state: 529.
##
## for_in -> For Ident ARROW . Ident In Map expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ARROW
##

Ill-formed bounded loop over a map.
At this point, an identifier matching values (as opposed to keys) in a
map is expected.

interactive_expr: Begin For Ident ASSIGN Verbatim To Verbatim Step Verbatim Attr
##
## Ends in an error in state: 526.
##
## for_int -> For Ident ASSIGN expr To expr step_clause . block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ASSIGN expr To expr step_clause
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 525, spurious reduction of production step_clause -> Step expr
##

Ill-formed bounded loop over a numerical interval.
At this point, if the step clause is complete, the loop body is
expected as a block of instructions.
Note: A block is either delimited by '{' and '}', or 'begin' and
'end'.

interactive_expr: Begin For Ident ASSIGN Verbatim To Verbatim Step With
##
## Ends in an error in state: 524.
##
## step_clause -> Step . expr [ LBRACE Block Begin ]
##
## The known suffix of the stack is as follows:
## Step
##

Ill-formed bounded loop over a numerical interval.
At this point, the index increment is expected as an expression.

interactive_expr: Begin For Ident ASSIGN Verbatim To With
##
## Ends in an error in state: 522.
##
## for_int -> For Ident ASSIGN expr To . expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_int -> For Ident ASSIGN expr To . expr step_clause block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ASSIGN expr To
##

Ill-formed bounded loop over a numerical interval.
At this point, the final value of the index is expected as an
expression.

interactive_expr: LBRACE For Ident ASSIGN Verbatim VBAR
##
## Ends in an error in state: 521.
##
## for_int -> For Ident ASSIGN expr . To expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_int -> For Ident ASSIGN expr . To expr step_clause block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ASSIGN expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

Ill-formed bounded loop over a numerical interval.
At this point, if the initial value of the index is complete, the
keyword 'to' is expected, followed by an expression whose value is the
final value of the index.

interactive_expr: Begin For Ident ASSIGN With
##
## Ends in an error in state: 520.
##
## for_int -> For Ident ASSIGN . expr To expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_int -> For Ident ASSIGN . expr To expr step_clause block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ASSIGN
##

Ill-formed bounded loop over a numerical interval.
At this point, the expression for the initial value of the index is
expected.

interactive_expr: LBRACE For Ident ARROW Ident In Map Verbatim VBAR
##
## Ends in an error in state: 533.
##
## for_in -> For Ident ARROW Ident In Map expr . block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ARROW Ident In Map expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

Ill-formed bounded loop over a list or a set.
At this point, if the collection is complete, the body of the loop is
expected as a block of instructions.
Note: A block is either delimited by '{' and '}', or 'begin' and
'end'.

interactive_expr: Begin For Ident In Set With
##
## Ends in an error in state: 517.
##
## for_in -> For Ident In collection . expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident In collection
##

Ill-formed bounded loop over a collection (set or list).
At this point, the collection is expected as an expression.

interactive_expr: Begin For Ident In With
##
## Ends in an error in state: 514.
##
## for_in -> For Ident In . collection expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident In
##

Ill-formed bounded loop over a collection (set or list).
At this point, the keyword 'set' or 'list' is expected, followed by
the actual collection as an expression.

interactive_expr: Begin For Ident With
##
## Ends in an error in state: 513.
##
## for_in -> For Ident . ARROW Ident In Map expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_in -> For Ident . In collection expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_int -> For Ident . ASSIGN expr To expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_int -> For Ident . ASSIGN expr To expr step_clause block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident
##

Ill-formed bounded loop.
At this point, one of the following is expected:
  * if iterating over a numerical interval, then the assignment
    operator ':=' is expected, followed by the initial value of the
    index as an expression;
  * if iterating over a list or a set, then the keyword 'in' is
    expected, followed by either the keyword 'list' or 'map';
  * if iterating over a map, then a binding arrow '->' followed by a
    variable matching the range of the map (that is, 'values', as
    opposed to 'keys').

interactive_expr: Begin For With
##
## Ends in an error in state: 512.
##
## for_in -> For . Ident ARROW Ident In Map expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_in -> For . Ident In collection expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_int -> For . Ident ASSIGN expr To expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_int -> For . Ident ASSIGN expr To expr step_clause block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For
##

Ill-formed bounded loop.
At this point, one of the following is expected:
  * if iterating over a numerical interval, then the index is expected
    as an identifier;
  * if iterating over a list or a set, then the element is expected as
    an identifier;
  * if iterating over a map, then the key (ranging over the domain of
    the map) is expected as an identifier.

interactive_expr: Begin Ident ASSIGN With
##
## Ends in an error in state: 556.
##
## assignment(expr) -> lhs ASSIGN . expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## lhs ASSIGN
##
interactive_expr: LBRACE If Verbatim Then Ident ASSIGN With
##
## Ends in an error in state: 591.
##
## assignment(closed_expr) -> lhs ASSIGN . closed_expr [ Else ]
## assignment(expr) -> lhs ASSIGN . expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## lhs ASSIGN
##

Ill-formed assignment.
At this point, the right-hand side of the assignment is expected as an
expression.

interactive_expr: Begin Ident With
##
## Ends in an error in state: 555.
##
## assignment(expr) -> lhs . ASSIGN expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## lhs
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 133, spurious reduction of production field_path -> Ident
## In state 275, spurious reduction of production local_path -> field_path
## In state 269, spurious reduction of production path_expr -> local_path
## In state 548, spurious reduction of production lhs -> path_expr
##
interactive_expr: LBRACE If Verbatim Then Ident With
##
## Ends in an error in state: 590.
##
## assignment(closed_expr) -> lhs . ASSIGN closed_expr [ Else ]
## assignment(expr) -> lhs . ASSIGN expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## lhs
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 133, spurious reduction of production field_path -> Ident
## In state 275, spurious reduction of production local_path -> field_path
## In state 269, spurious reduction of production path_expr -> local_path
## In state 548, spurious reduction of production lhs -> path_expr
##

Ill-formed assignment.
At this point, if the left-hand side of the assignment is complete,
the assignment operator ':=' is expected, followed by an expression.

interactive_expr: Begin If Verbatim Then LBRACE Skip With
##
## Ends in an error in state: 611.
##
## block -> option(Block) LBRACE sep_or_term_list(statement,SEMI) . RBRACE [ With VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## option(Block) LBRACE sep_or_term_list(statement,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 605, spurious reduction of production nsepseq(statement,SEMI) -> statement
## In state 614, spurious reduction of production sep_or_term_list(statement,SEMI) -> nsepseq(statement,SEMI)
##

Ill-formed block of statements.
At this point, if the statement is complete, one of the following is
expected:
  * a semicolon ';' followed by another statement (instruction or
    declaration);
  * a closing brace '}' if the block is complete.

interactive_expr: Begin If Verbatim Then Skip Else With
##
## Ends in an error in state: 603.
##
## if_then_else_instr(instruction) -> If expr Then test_clause(closed_instr) Else . test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If expr Then test_clause(closed_instr) Else
##
interactive_expr: LBRACE If Verbatim Then If Verbatim Then Skip Else With
##
## Ends in an error in state: 582.
##
## if_then_else_instr(closed_instr) -> If expr Then test_clause(closed_instr) Else . test_clause(closed_instr) [ Else ]
## if_then_else_instr(instruction) -> If expr Then test_clause(closed_instr) Else . test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If expr Then test_clause(closed_instr) Else
##

Ill-formed conditional instruction.
At this point, one of the following is expected:
  * a single instruction;
  * a block of statements (instructions and declarations).
Note: A block is either delimited by '{' and '}', or 'begin' and
'end'.

interactive_expr: Begin If Verbatim Then Skip With
##
## Ends in an error in state: 602.
##
## if_then_else_instr(instruction) -> If expr Then test_clause(closed_instr) . Else test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If expr Then test_clause(closed_instr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 475, spurious reduction of production base_instr(closed_instr,closed_expr) -> Skip
## In state 600, spurious reduction of production closed_instr -> base_instr(closed_instr,closed_expr)
## In state 596, spurious reduction of production test_clause(closed_instr) -> closed_instr
##
interactive_expr: LBRACE If Verbatim Then If Verbatim Then Skip With
##
## Ends in an error in state: 581.
##
## if_then_else_instr(closed_instr) -> If expr Then test_clause(closed_instr) . Else test_clause(closed_instr) [ Else ]
## if_then_else_instr(instruction) -> If expr Then test_clause(closed_instr) . Else test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If expr Then test_clause(closed_instr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 475, spurious reduction of production base_instr(closed_instr,closed_expr) -> Skip
## In state 600, spurious reduction of production closed_instr -> base_instr(closed_instr,closed_expr)
## In state 596, spurious reduction of production test_clause(closed_instr) -> closed_instr
##

Ill-formed conditional instruction.
At this point, if the 'then' branch is complete, the keyword 'else' is
expected, followed by one of the following:
  * a single instruction;
  * a block of statements (instructions and declarations).
Note: A block is either delimited by '{' and '}', or 'begin' and
'end'.

interactive_expr: Begin If Verbatim Then With
##
## Ends in an error in state: 474.
##
## if_then_else_instr(instruction) -> If expr Then . test_clause(closed_instr) Else test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
## if_then_instr(instruction) -> If expr Then . test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If expr Then
##
interactive_expr: LBRACE If Verbatim Then If Verbatim Then With
##
## Ends in an error in state: 511.
##
## if_then_else_instr(closed_instr) -> If expr Then . test_clause(closed_instr) Else test_clause(closed_instr) [ Else ]
## if_then_else_instr(instruction) -> If expr Then . test_clause(closed_instr) Else test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
## if_then_instr(instruction) -> If expr Then . test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If expr Then
##

Ill-formed conditional instruction.
At this point, one of the following is expected for the contents of
the 'then' branch:
  * a single instruction;
  * a block of statements (instructions and declarations).
Note: A block is either delimited by '{' and '}', or 'begin' and
'end'.

interactive_expr: LBRACE If Verbatim VBAR
##
## Ends in an error in state: 473.
##
## if_then_else_instr(instruction) -> If expr . Then test_clause(closed_instr) Else test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
## if_then_instr(instruction) -> If expr . Then test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##
interactive_expr: LBRACE If Verbatim Then If Verbatim VBAR
##
## Ends in an error in state: 510.
##
## if_then_else_instr(closed_instr) -> If expr . Then test_clause(closed_instr) Else test_clause(closed_instr) [ Else ]
## if_then_else_instr(instruction) -> If expr . Then test_clause(closed_instr) Else test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
## if_then_instr(instruction) -> If expr . Then test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

Ill-formed conditional instruction.
At this point, if the condition is complete, the keyword 'then' is
expected, followed by one of the following:
  * a single instruction;
  * a block of statements (instructions and declarations).
Note: A block is either delimited by '{' and '}', or 'begin' and
'end'.

interactive_expr: Begin If With
##
## Ends in an error in state: 472.
##
## if_then_else_instr(instruction) -> If . expr Then test_clause(closed_instr) Else test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
## if_then_instr(instruction) -> If . expr Then test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If
##
interactive_expr: LBRACE If Verbatim Then If With
##
## Ends in an error in state: 509.
##
## if_then_else_instr(closed_instr) -> If . expr Then test_clause(closed_instr) Else test_clause(closed_instr) [ Else ]
## if_then_else_instr(instruction) -> If . expr Then test_clause(closed_instr) Else test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
## if_then_instr(instruction) -> If . expr Then test_clause(instruction) [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## If
##

Ill-formed conditional instruction.
At this point, a condition is expected as an expression.

interactive_expr: Ident DOT Int DOT With
##
## Ends in an error in state: 138.
##
## nsepseq(selection,DOT) -> selection DOT . nsepseq(selection,DOT) [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## selection DOT
##
interactive_expr: Ident DOT With
##
## Ends in an error in state: 134.
##
## field_path -> Ident DOT . nsepseq(selection,DOT) [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Ident DOT
##

Ill-formed selection in a record or a tuple.
At this point, one of the following is expected:
  * a record field name, if selecting from a record;
  * the index of a tuple component, '0' denoting the first component.

interactive_expr: Begin Patch Ident While
##
## Ends in an error in state: 376.
##
## map_patch -> Patch core_expr . With ne_injection(Map,binding) [ VBAR SEMI RBRACKET RBRACE End Else ]
## record_patch -> Patch core_expr . With record_expr [ VBAR SEMI RBRACKET RBRACE End Else ]
## set_patch -> Patch core_expr . With ne_injection(Set,expr) [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Patch core_expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 133, spurious reduction of production field_path -> Ident
## In state 275, spurious reduction of production local_path -> field_path
## In state 269, spurious reduction of production path_expr -> local_path
## In state 168, spurious reduction of production core_expr -> path_expr
##

Ill-formed patch.
At this point, if the expression denoting the data structure (map, set
or record) being patched is compled, the keyword 'with' is expected,
followed by a map, a set or a record expression.

interactive_expr: LBRACE Patch Ident With Map LBRACKET Verbatim ARROW Bytes VBAR
##
## Ends in an error in state: 392.
##
## ne_injection(Map,binding) -> Map LBRACKET sep_or_term_list(binding,SEMI) . RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Map LBRACKET sep_or_term_list(binding,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 398, spurious reduction of production binding -> expr ARROW expr
## In state 399, spurious reduction of production nsepseq(binding,SEMI) -> binding
## In state 395, spurious reduction of production sep_or_term_list(binding,SEMI) -> nsepseq(binding,SEMI)
##

Ill-formed map patch.
At this point, if the key/value binding is complete, one of the
following is expected:
  * a semicolon ';' followed by another key/value binding;
  * a closing bracket ']' if the map is complete.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Begin Patch Ident With Map LBRACKET With
##
## Ends in an error in state: 391.
##
## ne_injection(Map,binding) -> Map LBRACKET . sep_or_term_list(binding,SEMI) RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Map LBRACKET
##

Ill-formed map patch.
At this point, key/value bindings are expected, separated by
semicolons ';' and followed by a closing bracket ']'.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Begin Patch Ident With Map With
##
## Ends in an error in state: 390.
##
## ne_injection(Map,binding) -> Map . LBRACKET sep_or_term_list(binding,SEMI) RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
## ne_injection(Map,binding) -> Map . sep_or_term_list(binding,SEMI) End [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Map
##

Ill-formed map patch.
At this point, one of the following is expected:
  * key/value bindings (updates) separated by semicolons ';' and
    followed by the keyword 'end';
  * an opening bracket '[' followed by key/value bindings and a
    closing bracket ']'.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Record With
##
## Ends in an error in state: 118.
##
## ne_injection(Record,field_assignment) -> Record . LBRACKET sep_or_term_list(field_assignment,SEMI) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
## ne_injection(Record,field_assignment) -> Record . sep_or_term_list(field_assignment,SEMI) End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Record
##

Ill-formed record expression.
At this point, one of the following is expected:
  * assignments to record fields, separated by semicolons
    ';' and followed by the keyword 'end';
  * an opening bracket '[' followed by assignments and a closing
    bracket ']'.

interactive_expr: Begin Patch Ident With Set LBRACKET With
##
## Ends in an error in state: 379.
##
## ne_injection(Set,expr) -> Set LBRACKET . sep_or_term_list(expr,SEMI) RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Set LBRACKET
##

Ill-formed set patch.
At this point, set elements (updates) separated by semicolons ';' are
expected and followed by a closing brace ']'.

interactive_expr: Begin Patch Ident With Set With
##
## Ends in an error in state: 378.
##
## ne_injection(Set,expr) -> Set . LBRACKET sep_or_term_list(expr,SEMI) RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
## ne_injection(Set,expr) -> Set . sep_or_term_list(expr,SEMI) End [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Set
##

Ill-formed set of expressions.
At this point, one of the following is expected:
  * set elements separated by semicolons ';' and followed by the
    keyword 'end';
  * an opening bracket '[' followed by set elements and a closing
    bracket ']'.

interactive_expr: Begin Patch Ident With With
##
## Ends in an error in state: 377.
##
## map_patch -> Patch core_expr With . ne_injection(Map,binding) [ VBAR SEMI RBRACKET RBRACE End Else ]
## record_patch -> Patch core_expr With . record_expr [ VBAR SEMI RBRACKET RBRACE End Else ]
## set_patch -> Patch core_expr With . ne_injection(Set,expr) [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Patch core_expr With
##

Ill-formed patch.
At this point, a map, a record or a set is expected, starting with the
keywords 'map', 'record' or 'set', respectively.

interactive_expr: Begin Patch With
##
## Ends in an error in state: 375.
##
## map_patch -> Patch . core_expr With ne_injection(Map,binding) [ VBAR SEMI RBRACKET RBRACE End Else ]
## record_patch -> Patch . core_expr With record_expr [ VBAR SEMI RBRACKET RBRACE End Else ]
## set_patch -> Patch . core_expr With ne_injection(Set,expr) [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Patch
##

Ill-formed patch.
At this point, an expression is expected to denote a map, a record or a set.

interactive_expr: Begin Remove Verbatim From Map With
##
## Ends in an error in state: 373.
##
## map_remove(expr) -> Remove expr From Map . expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove expr From Map
##

Ill-formed removal of a binding in a map.
At this point, an expression is expected to denote a map.

interactive_expr: Begin Remove Verbatim From Set With
##
## Ends in an error in state: 371.
##
## set_remove(expr) -> Remove expr From Set . expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove expr From Set
##

Ill-formed removal of an element in a set.
At this point, an expression is expected to denote a set.

interactive_expr: Begin Remove Verbatim From With
##
## Ends in an error in state: 370.
##
## map_remove(expr) -> Remove expr From . Map expr [ VBAR SEMI RBRACKET RBRACE End ]
## set_remove(expr) -> Remove expr From . Set expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove expr From
##

Ill-formed removal in a map or a set.
At this point, either the keyword 'map' or 'set' is expected.

interactive_expr: Begin Remove With
##
## Ends in an error in state: 368.
##
## map_remove(expr) -> Remove . expr From Map expr [ VBAR SEMI RBRACKET RBRACE End ]
## set_remove(expr) -> Remove . expr From Set expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove
##

Ill-formed removal from a map or a set.
At this point, one of the following is expected:
  * if removing an element from a set, that set element as an
    expression;
  * if removing a binding from a map, the key of that binding as an
    expression.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Begin Skip End While
##
## Ends in an error in state: 348.
##
## block_with(expr) -> block . With expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## block
##
interactive_expr: If Verbatim Then LBRACE Skip RBRACE While
##
## Ends in an error in state: 493.
##
## block_with(closed_expr) -> block . With closed_expr [ Else ]
## block_with(expr) -> block . With expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## block
##

Ill-formed block of instructions.
At this point, the keyword 'with' is expected, followed by an
expression whose value is that of the whole block.

interactive_expr: Begin Skip End With With
##
## Ends in an error in state: 349.
##
## block_with(expr) -> block With . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## block With
##
interactive_expr: If Verbatim Then LBRACE Skip RBRACE With With
##
## Ends in an error in state: 494.
##
## block_with(closed_expr) -> block With . closed_expr [ Else ]
## block_with(expr) -> block With . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## block With
##

Ill-formed block of instructions.
At this point, an expression is expected, whose value is that of the
block.

interactive_expr: Begin Skip With
##
## Ends in an error in state: 619.
##
## block -> Begin sep_or_term_list(statement,SEMI) . End [ With VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Begin sep_or_term_list(statement,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 605, spurious reduction of production nsepseq(statement,SEMI) -> statement
## In state 614, spurious reduction of production sep_or_term_list(statement,SEMI) -> nsepseq(statement,SEMI)
##

Ill-formed block of statements.
At this point, if the statement is complete, one of the following is
expected:
  * a semicolon ';' followed by another statement (instruction or
    declaration);
  * the keyword 'end' if the block is complete.

interactive_expr: Begin Var Ident ASSIGN With
##
## Ends in an error in state: 260.
##
## unqualified_decl(ASSIGN) -> core_pattern ASSIGN . expr [ SEMI RBRACE End ]
##
## The known suffix of the stack is as follows:
## core_pattern ASSIGN
##

Ill-formed variable declaration.
At this point, an expression is expected, whose value is the initial
value of the variable being defined.

interactive_expr: Begin Var Ident COLON UIdent ARROW
##
## Ends in an error in state: 356.
##
## unqualified_decl(ASSIGN) -> core_pattern type_annotation . ASSIGN expr [ SEMI RBRACE End ]
##
## The known suffix of the stack is as follows:
## core_pattern type_annotation
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 15, spurious reduction of production variant -> UIdent
## In state 32, spurious reduction of production nsepseq(variant,VBAR) -> variant
## In state 62, spurious reduction of production sum_type -> nsepseq(variant,VBAR)
## In state 58, spurious reduction of production type_expr -> sum_type
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
##

Ill-formed variable declaration.
At this point, if the type annotation is complete, the assignment
operator ':=' is expected, followed by the expression whose value is
the initial value of the variable being defined.

interactive_expr: Begin Var Ident With
##
## Ends in an error in state: 259.
##
## unqualified_decl(ASSIGN) -> core_pattern . ASSIGN expr [ SEMI RBRACE End ]
## unqualified_decl(ASSIGN) -> core_pattern . type_annotation ASSIGN expr [ SEMI RBRACE End ]
##
## The known suffix of the stack is as follows:
## core_pattern
##

Ill-formed variable declaration.
At this point, one of the following is expected:
  * a type annotation introduced by a colon ':' (to form a typed
    pattern);
  * the assignment operator ':=' followed by an expression whose
    value is the initial value of the variable being defined.

interactive_expr: Begin Var With
##
## Ends in an error in state: 179.
##
## var_decl -> Var . unqualified_decl(ASSIGN) [ SEMI RBRACE End ]
##
## The known suffix of the stack is as follows:
## Var
##

Ill-formed variable declaration.
At this point, a pattern is expected, for instance the name of the
variable being defined, or the wildcard pattern '_'.

interactive_expr: LBRACE While Verbatim VBAR
##
## Ends in an error in state: 617.
##
## while_loop -> While expr . block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## While expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

Ill-formed general loop.
At this point, if the condition is complete, the body of the loop is
expected as a block of instructions.
Note: A block is either delimited by '{' and '}', or 'begin' and
'end'.

interactive_expr: Begin While With
##
## Ends in an error in state: 155.
##
## while_loop -> While . expr block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## While
##

Ill-formed general loop.
At this point, the condition is expected as an expression.

interactive_expr: BigMap LBRACKET With
##
## Ends in an error in state: 153.
##
## injection(BigMap,binding) -> BigMap LBRACKET . option(sep_or_term_list(binding,SEMI)) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## BigMap LBRACKET
##

Ill-formed big_map expression.
At this point, one of the following is expected:
  * key/value bindings separated by semicolons ';' and concluded by a
    closing bracket ']';
  * a closing bracket ']' if the big_map is empty.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: BigMap With
##
## Ends in an error in state: 152.
##
## injection(BigMap,binding) -> BigMap . LBRACKET option(sep_or_term_list(binding,SEMI)) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
## injection(BigMap,binding) -> BigMap . option(sep_or_term_list(binding,SEMI)) End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## BigMap
##

Ill-formed big_map expression.
At this point, one of the following is expected:
  * key/value bindings separated by semicolons ';' and concluded by
    the keyword 'end';
  * an opening bracket '[' followed by key/value bindings,
    or none (empty big_map), and a closing bracket ']';
  * the keyword 'end' if the big_map is empty.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Block With
##
## Ends in an error in state: 177.
##
## block -> option(Block) . LBRACE sep_or_term_list(statement,SEMI) RBRACE [ With VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## option(Block)
##

Ill-formed block of statements.
At this point, an opening brace '{' is expected, followed by
statements (instructions and declarations) separated or terminated by
semicolons ';', and concluded by a closing brace '}'.

interactive_expr: Ctor_Some With
##
## Ends in an error in state: 145.
##
## some_expr -> Ctor_Some . arguments [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Ctor_Some
##

Ill-formed application of the constructor 'Some'.
At this point, the arguments are expected as a tuple of expressions.

interactive_expr: Case Verbatim Of Ctor_Some LPAR WILD With
##
## Ends in an error in state: 212.
##
## par(core_pattern) -> LPAR core_pattern . RPAR [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR core_pattern
##

Ill-formed application of the constructor 'Some' in a pattern.
At this point, if the pattern matching the argument(s) of 'Some' is
complete, a closing parenthesis ')' is expected.

interactive_expr: Case Verbatim Of Ctor_Some LPAR With
##
## Ends in an error in state: 198.
##
## par(core_pattern) -> LPAR . core_pattern RPAR [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR
##

Ill-formed application of the constructor 'Some' in a pattern.
At this point, a pattern that matches the argument(s) of 'Some' is
expected.

interactive_expr: Case Verbatim Of Ctor_Some With
##
## Ends in an error in state: 197.
##
## some_pattern -> Ctor_Some . par(core_pattern) [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Ctor_Some
##

Ill-formed application of the constructor 'Some' in a pattern.
At this point, a pattern that matches the argument(s) of 'Some' is
expected between parentheses.

interactive_expr: Case Verbatim Of UIdent LPAR With
##
## Ends in an error in state: 183.
##
## par(tuple(pattern)) -> LPAR . tuple(pattern) RPAR [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR
##

Ill-formed tuple of patterns.
At this point, the first component is expected as a pattern.

interactive_expr: Case Verbatim Of LBRACKET WILD ARROW Bytes Type
##
## Ends in an error in state: 634.
##
## case(expr) -> Case expr Of LBRACKET option(VBAR) cases(expr) . RBRACKET [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End Else EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Case expr Of LBRACKET option(VBAR) cases(expr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 632, spurious reduction of production case_clause(expr) -> pattern ARROW expr
## In state 636, spurious reduction of production nsepseq(case_clause(expr),VBAR) -> case_clause(expr)
## In state 633, spurious reduction of production cases(expr) -> nsepseq(case_clause(expr),VBAR)
##

Ill-formed case expression.
At this point, if the case clause is complete, one of the following is
expected:
  * more clauses, separated by vertical bars '|';
  * a closing bracket ']' if the case is complete.
Note: A clause, here, is made of a pattern followed by an arrow '->'
and an expression.

interactive_expr: Case Verbatim Of LBRACKET With
##
## Ends in an error in state: 629.
##
## case(expr) -> Case expr Of LBRACKET option(VBAR) . cases(expr) RBRACKET [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End Else EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Case expr Of LBRACKET option(VBAR)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 628, spurious reduction of production option(VBAR) ->
##
interactive_expr: Case Verbatim Of With
##
## Ends in an error in state: 639.
##
## case(expr) -> Case expr Of option(VBAR) . cases(expr) End [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End Else EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Case expr Of option(VBAR)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 627, spurious reduction of production option(VBAR) ->
##
interactive_expr: Case Verbatim Of WILD ARROW Bytes VBAR With
##
## Ends in an error in state: 637.
##
## nsepseq(case_clause(expr),VBAR) -> case_clause(expr) VBAR . nsepseq(case_clause(expr),VBAR) [ RBRACKET End ]
##
## The known suffix of the stack is as follows:
## case_clause(expr) VBAR
##

Ill-formed case expression.
At this point, a case clause is expected, made of a pattern and an
expression separated by an arrow '->'.

interactive_expr: Case Verbatim Of LPAR WILD COMMA With
##
## Ends in an error in state: 221.
##
## tuple(pattern) -> pattern COMMA . nsepseq(pattern,COMMA) [ RPAR ]
##
## The known suffix of the stack is as follows:
## pattern COMMA
##

Ill-formed tuple of patterns.
At this point, a tuple component is expected as a pattern.

interactive_expr: Case Verbatim Of LPAR WILD SHARP Bytes With
##
## Ends in an error in state: 219.
##
## par(pattern) -> LPAR pattern . RPAR [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
## tuple(pattern) -> pattern . COMMA nsepseq(pattern,COMMA) [ RPAR ]
## typed_pattern -> pattern . type_annotation [ RPAR ]
##
## The known suffix of the stack is as follows:
## LPAR pattern
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 228, spurious reduction of production nsepseq(core_pattern,SHARP) -> core_pattern
## In state 227, spurious reduction of production pattern -> core_pattern SHARP nsepseq(core_pattern,SHARP)
##

Ill-formed pattern.
At this point, if the pattern is complete, one of the following is
expected, depending on the kind of pattern you want:
  * a closing parenthesis ')', if a parenthesised pattern;
  * a comma ',' followed by another pattern, if a tuple pattern;
  * a type annotation starting with a colon ':', if a typed pattern.

interactive_expr: Case Verbatim Of LPAR With
##
## Ends in an error in state: 193.
##
## par(pattern) -> LPAR . pattern RPAR [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
## par(tuple(pattern)) -> LPAR . tuple(pattern) RPAR [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
## par(typed_pattern) -> LPAR . typed_pattern RPAR [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR
##

Ill-formed pattern.
At this point, a pattern is expected.

interactive_expr: Case Verbatim Of List LBRACKET WILD With
##
## Ends in an error in state: 235.
##
## injection(List,core_pattern) -> List LBRACKET option(sep_or_term_list(core_pattern,SEMI)) . RBRACKET [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## List LBRACKET option(sep_or_term_list(core_pattern,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 239, spurious reduction of production nsepseq(core_pattern,SEMI) -> core_pattern
## In state 238, spurious reduction of production sep_or_term_list(core_pattern,SEMI) -> nsepseq(core_pattern,SEMI)
## In state 234, spurious reduction of production option(sep_or_term_list(core_pattern,SEMI)) -> sep_or_term_list(core_pattern,SEMI)
##

Ill-formed list of patterns.
At this point, if the pattern that matches a list element is complete,
one of the following is expected:
  * a semicolon ';' followed by a pattern for the next element;
  * a closing bracket ']' if the list is complete.

interactive_expr: Case Verbatim Of List LBRACKET With
##
## Ends in an error in state: 233.
##
## injection(List,core_pattern) -> List LBRACKET . option(sep_or_term_list(core_pattern,SEMI)) RBRACKET [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## List LBRACKET
##

Ill-formed list of patterns.
At this point, one of the following is expected:
  * patterns that match list elements, separated by semicolons ';';
  * a closing bracket ']' if matching the empty list.

interactive_expr: Case Verbatim Of List With
##
## Ends in an error in state: 192.
##
## injection(List,core_pattern) -> List . LBRACKET option(sep_or_term_list(core_pattern,SEMI)) RBRACKET [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
## injection(List,core_pattern) -> List . option(sep_or_term_list(core_pattern,SEMI)) End [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## List
##

Ill-formed list of patterns.
At this point, one of the following is expected, depending on the
dialect you are using (terse or verbose):
  * patterns that match list elements, separated by semicolons ';'
    (verbose);
  * an opening bracket '[' followed by patterns separated by
    semicolons ';', and a closing bracket ']' (terse);
  * the brackets '[]' (terse) or the keyword 'end' (verbose), if
    matching the empty list.

interactive_expr: Case Verbatim Of WILD ARROW With
##
## Ends in an error in state: 631.
##
## case_clause(expr) -> pattern ARROW . expr [ VBAR RBRACKET End ]
##
## The known suffix of the stack is as follows:
## pattern ARROW
##

Ill-formed clause of a case expression.
At this point, the right-hand side of the clause is expected as an
expression.
Note: A clause, here, is made of a pattern and an expression separated
by an arrow '->'.

interactive_expr: Case Verbatim Of WILD SHARP WILD SHARP With
##
## Ends in an error in state: 229.
##
## nsepseq(core_pattern,SHARP) -> core_pattern SHARP . nsepseq(core_pattern,SHARP) [ RPAR COMMA COLON ARROW ]
##
## The known suffix of the stack is as follows:
## core_pattern SHARP
##
interactive_expr: Case Verbatim Of WILD SHARP With
##
## Ends in an error in state: 226.
##
## pattern -> core_pattern SHARP . nsepseq(core_pattern,SHARP) [ RPAR COMMA COLON ARROW ]
##
## The known suffix of the stack is as follows:
## core_pattern SHARP
##

Ill-formed list of patterns.
At this point, a pattern that matches a non-empty list is expected.

interactive_expr: Case Verbatim Of WILD With
##
## Ends in an error in state: 630.
##
## case_clause(expr) -> pattern . ARROW expr [ VBAR RBRACKET End ]
##
## The known suffix of the stack is as follows:
## pattern
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 225, spurious reduction of production pattern -> core_pattern
##

Ill-formed case clause.
At this point, if the pattern is complete, an arrow '->' is expected,
followed by the right-hand side as an expression.

interactive_expr: Case With
##
## Ends in an error in state: 149.
##
## case(expr) -> Case . expr Of LBRACKET option(VBAR) cases(expr) RBRACKET [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End Else EOF Directive Const COMMA Block Begin Attr ARROW ]
## case(expr) -> Case . expr Of option(VBAR) cases(expr) End [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End Else EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Case
##

Ill-formed case expression.
At this point, the condition is expected as an expression.

interactive_expr: UIdent DOT With
##
## Ends in an error in state: 360.
##
## module_path(field_path) -> UIdent DOT . module_path(field_path) [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
## module_path(field_path) -> UIdent DOT . field_path [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
## value_in_module -> UIdent DOT . Or [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
## value_in_module -> UIdent DOT . And [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## UIdent DOT
##

Ill-formed selection of a value from a module.
At this point, the qualified name of a value is expected (that is, an
access path to that value), starting with the name of a value defined
in the module.

interactive_expr: Function LPAR Const Ident RPAR COLON Ident With
##
## Ends in an error in state: 642.
##
## fun_expr(expr) -> Function parameters type_annotation . Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function parameters type_annotation
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 25, spurious reduction of production core_type -> Ident
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
##
interactive_expr: If Verbatim Then Function LPAR Const Ident RPAR COLON Ident With
##
## Ends in an error in state: 497.
##
## fun_expr(closed_expr) -> Function parameters type_annotation . Is closed_expr [ Else ]
## fun_expr(expr) -> Function parameters type_annotation . Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function parameters type_annotation
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 25, spurious reduction of production core_type -> Ident
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
##

Ill-formed function expression.
At this point, the keyword 'is' is expected, followed with the
function body as an expression or a block of statements (instructions
or declarations).
Note: A block, in this context, is delimited by '{' and '} with' or
'begin' and 'end with'.

interactive_expr: If Verbatim Then Function LPAR Const Ident RPAR Is With
##
## Ends in an error in state: 485.
##
## fun_expr(closed_expr) -> Function parameters Is . closed_expr [ Else ]
## fun_expr(expr) -> Function parameters Is . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function parameters Is
##
interactive_expr: Function LPAR Const Ident RPAR Is With
##
## Ends in an error in state: 143.
##
## fun_expr(expr) -> Function parameters Is . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function parameters Is
##

Ill-formed function expression.
At this point, the function body is expected as an expression.

interactive_expr: Function LPAR Const Ident RPAR With
##
## Ends in an error in state: 142.
##
## fun_expr(expr) -> Function parameters . Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## fun_expr(expr) -> Function parameters . type_annotation Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function parameters
##
contract: Attr Function Ident LPAR Const Ident RPAR With
##
## Ends in an error in state: 446.
##
## fun_decl -> nseq(Attr) Function Ident parameters . Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) Function Ident parameters . type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Function Ident parameters
##
interactive_expr: If Verbatim Then Function LPAR Const Ident RPAR With
##
## Ends in an error in state: 484.
##
## fun_expr(closed_expr) -> Function parameters . Is closed_expr [ Else ]
## fun_expr(closed_expr) -> Function parameters . type_annotation Is closed_expr [ Else ]
## fun_expr(expr) -> Function parameters . Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## fun_expr(expr) -> Function parameters . type_annotation Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function parameters
##

Ill-formed function expression.
At this point, one of the following is expected:
  * a type annotation for the returned value, starting with a colon ':';
  * the keyword 'is' followed by the function body as an expression.

interactive_expr: Function LPAR Const Ident With
##
## Ends in an error in state: 102.
##
## param_decl -> Const Ident . option(type_annotation) [ SEMI RPAR ]
##
## The known suffix of the stack is as follows:
## Const Ident
##
interactive_expr: Function LPAR Var Ident With
##
## Ends in an error in state: 97.
##
## param_decl -> Var Ident . option(type_annotation) [ SEMI RPAR ]
##
## The known suffix of the stack is as follows:
## Var Ident
##

Ill-formed function parameter declaration.
At this point, one of the following is expected:
  * a type annotation starting with a colon ':';
  * a semicolon ';' followed with another parameter declaration;
  * a closing parenthesis ')' if there are no more parameters.

interactive_expr: Function LPAR Var Ident COLON String With
##
## Ends in an error in state: 107.
##
## par(nsepseq(param_decl,SEMI)) -> LPAR nsepseq(param_decl,SEMI) . RPAR [ Is COLON ]
##
## The known suffix of the stack is as follows:
## LPAR nsepseq(param_decl,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
## In state 95, spurious reduction of production option(type_annotation) -> type_annotation
## In state 98, spurious reduction of production param_decl -> Var Ident option(type_annotation)
## In state 104, spurious reduction of production nsepseq(param_decl,SEMI) -> param_decl
##

Ill-formed function parameter declaration.
At this point, if the parameter declaration is complete, one of the
following is expected:
  * a closing parenthesis ')' followed by the keyword 'is';
  * a type annotation starting with a colon ':' and the type of the
    values returned by the function being defined.

interactive_expr: Function LPAR Var Ident SEMI With
##
## Ends in an error in state: 105.
##
## nsepseq(param_decl,SEMI) -> param_decl SEMI . nsepseq(param_decl,SEMI) [ RPAR ]
##
## The known suffix of the stack is as follows:
## param_decl SEMI
##

Ill-formed function parameters.
At this point, a function parameter declaration is expected.

interactive_expr: Function LPAR Var With
##
## Ends in an error in state: 93.
##
## param_decl -> Var . Ident option(type_annotation) [ SEMI RPAR ]
## param_decl -> Var . WILD option(type_annotation) [ SEMI RPAR ]
##
## The known suffix of the stack is as follows:
## Var
##
interactive_expr: Function LPAR Const With
##
## Ends in an error in state: 99.
##
## param_decl -> Const . Ident option(type_annotation) [ SEMI RPAR ]
## param_decl -> Const . WILD option(type_annotation) [ SEMI RPAR ]
##
## The known suffix of the stack is as follows:
## Const
##

Ill-formed function parameter declaration.
At this point, one of the following is expected:
  * the name of the parameter;
  * the wildcard pattern '_'.

interactive_expr: Function LPAR With
##
## Ends in an error in state: 92.
##
## par(nsepseq(param_decl,SEMI)) -> LPAR . nsepseq(param_decl,SEMI) RPAR [ Is COLON ]
##
## The known suffix of the stack is as follows:
## LPAR
##

Ill-formed function parameter declaration.
At this point, a parameter declaration is expected.

interactive_expr: Function With
##
## Ends in an error in state: 141.
##
## fun_expr(expr) -> Function . parameters Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## fun_expr(expr) -> Function . parameters type_annotation Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function
##
interactive_expr: If Verbatim Then Function With
##
## Ends in an error in state: 483.
##
## fun_expr(closed_expr) -> Function . parameters Is closed_expr [ Else ]
## fun_expr(closed_expr) -> Function . parameters type_annotation Is closed_expr [ Else ]
## fun_expr(expr) -> Function . parameters Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## fun_expr(expr) -> Function . parameters type_annotation Is expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function
##

Ill-formed function expression.
At this point, parameter declarations are expected between parentheses.

interactive_expr: Ident LBRACKET With
##
## Ends in an error in state: 169.
##
## brackets(expr) -> LBRACKET . expr RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LBRACKET
##

Ill-formed map lookup.
At this point, a key is expected as an expression.

interactive_expr: Ident LPAR Verbatim COMMA With
##
## Ends in an error in state: 656.
##
## nsepseq(expr,COMMA) -> expr COMMA . nsepseq(expr,COMMA) [ RPAR ]
##
## The known suffix of the stack is as follows:
## expr COMMA
##

Ill-formed tuple expression.
At this point, another component is expected as an expression.

interactive_expr: UIdent LPAR Verbatim VBAR
##
## Ends in an error in state: 691.
##
## par(nsepseq(expr,COMMA)) -> LPAR nsepseq(expr,COMMA) . RPAR [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR nsepseq(expr,COMMA)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 655, spurious reduction of production nsepseq(expr,COMMA) -> expr
##

Ill-formed function call.
At this point, if the argument is complete, a closing parenthesis ')'
is expected.

interactive_expr: Ident LPAR With
##
## Ends in an error in state: 114.
##
## par(nsepseq(expr,COMMA)) -> LPAR . nsepseq(expr,COMMA) RPAR [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR
##

Ill-formed function call.
At this point, an argument is expected as an expression.

interactive_expr: Ident With Record Ident EQ Bytes SEMI With
##
## Ends in an error in state: 352.
##
## ne_injection(Record,field_path_assignment) -> Record sep_or_term_list(field_path_assignment,SEMI) . End [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Record sep_or_term_list(field_path_assignment,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 287, spurious reduction of production nseq(__anonymous_0(field_path_assignment,SEMI)) -> field_path_assignment SEMI
## In state 284, spurious reduction of production sep_or_term_list(field_path_assignment,SEMI) -> nseq(__anonymous_0(field_path_assignment,SEMI))
##

Ill-formed record update.
At this point, if the field update is complete, one of the following
is expected:
  * a semicolon ';' followed by another field update (assignment);
  * the keyword 'end' if the record update is complete.

interactive_expr: Ident With Record Ident With
##
## Ends in an error in state: 290.
##
## field_path_assignment -> field_path . EQ expr [ SEMI RBRACKET End ]
##
## The known suffix of the stack is as follows:
## field_path
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 133, spurious reduction of production field_path -> Ident
##

Ill-formed record update.
At this point, if the field to update is fully qualified, the symbol
'=' is expected, followed by an expression whose value is the new
value of the field.

interactive_expr: Ident With Record LBRACKET With
##
## Ends in an error in state: 281.
##
## ne_injection(Record,field_path_assignment) -> Record LBRACKET . sep_or_term_list(field_path_assignment,SEMI) RBRACKET [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Record LBRACKET
##

Ill-formed record update.
At this point, a field assignment (update) is expected.

interactive_expr: Ident With Record With
##
## Ends in an error in state: 280.
##
## ne_injection(Record,field_path_assignment) -> Record . LBRACKET sep_or_term_list(field_path_assignment,SEMI) RBRACKET [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
## ne_injection(Record,field_path_assignment) -> Record . sep_or_term_list(field_path_assignment,SEMI) End [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Record
##

Ill-formed record update.
At this point, one of the following is expected:
  * a series of field assignments (updates), separated by semicolons ';';
  * an opening bracket '[' followed by field assignments and a closing
    bracket ']'.

interactive_expr: Ident With With
##
## Ends in an error in state: 279.
##
## record_update -> core_expr With . ne_injection(Record,field_path_assignment) [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## core_expr With
##

Ill-formed record update.
At this point, a record is expected, starting with the keyword
'record'.

interactive_expr: If Verbatim Then Verbatim Else With
##
## Ends in an error in state: 647.
##
## if_then_else_expr(expr) -> If expr Then closed_expr Else . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## If expr Then closed_expr Else
##
interactive_expr: If Verbatim Then If Verbatim Then Verbatim Else With
##
## Ends in an error in state: 503.
##
## if_then_else_expr(closed_expr) -> If expr Then closed_expr Else . closed_expr [ Else ]
## if_then_else_expr(expr) -> If expr Then closed_expr Else . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## If expr Then closed_expr Else
##

Ill-formed conditional expression.
At this point, the contents of the 'else' branch is expected as an
expression.

interactive_expr: If Verbatim Then With
##
## Ends in an error in state: 645.
##
## if_then_else_expr(expr) -> If expr Then . closed_expr Else expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## if_then_expr(expr) -> If expr Then . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## If expr Then
##
interactive_expr: If Verbatim Then If Verbatim Then With
##
## Ends in an error in state: 482.
##
## if_then_else_expr(closed_expr) -> If expr Then . closed_expr Else closed_expr [ Else ]
## if_then_else_expr(expr) -> If expr Then . closed_expr Else expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## if_then_expr(expr) -> If expr Then . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## If expr Then
##

Ill-formed conditional expression.
At this point, the contents of the 'then' branch is expected as an
expression.

interactive_expr: If Verbatim Then If Verbatim VBAR
##
## Ends in an error in state: 481.
##
## if_then_else_expr(closed_expr) -> If expr . Then closed_expr Else closed_expr [ Else ]
## if_then_else_expr(expr) -> If expr . Then closed_expr Else expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## if_then_expr(expr) -> If expr . Then expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## If expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

Ill-formed conditional expression.
At this point, if the condition is complete, the keyword 'then' is
expected, followed by an expression.

interactive_expr: If With
##
## Ends in an error in state: 132.
##
## if_then_else_expr(expr) -> If . expr Then closed_expr Else expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## if_then_expr(expr) -> If . expr Then expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## If
##
interactive_expr: If Verbatim Then If With
##
## Ends in an error in state: 480.
##
## if_then_else_expr(closed_expr) -> If . expr Then closed_expr Else closed_expr [ Else ]
## if_then_else_expr(expr) -> If . expr Then closed_expr Else expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## if_then_expr(expr) -> If . expr Then expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## If
##

Ill-formed conditional expression.
At this point, the condition is expected as an expression.

interactive_expr: LPAR Verbatim COLON Ident With
##
## Ends in an error in state: 648.
##
## par(typed_expr) -> LPAR typed_expr . RPAR [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR typed_expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 25, spurious reduction of production core_type -> Ident
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
## In state 659, spurious reduction of production typed_expr -> disj_expr_level type_annotation
##

Ill-formed annotated expression.
At this point, if the type annotation is complete, a closing
parenthesis ')' is expected.

interactive_expr: LPAR Verbatim COMMA Bytes VBAR
##
## Ends in an error in state: 650.
##
## par(tuple(expr)) -> LPAR tuple(expr) . RPAR [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR tuple(expr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 655, spurious reduction of production nsepseq(expr,COMMA) -> expr
## In state 654, spurious reduction of production tuple(expr) -> expr COMMA nsepseq(expr,COMMA)
##

Ill-formed tuple expression.
At this point, if the component is complete, one of the following is
expected:
  * a comma ',' followed by another component as an expression;
  * a closing parenthesis ')' if the tuple is complete.

interactive_expr: LPAR Verbatim COMMA With
##
## Ends in an error in state: 653.
##
## tuple(expr) -> expr COMMA . nsepseq(expr,COMMA) [ RPAR ]
##
## The known suffix of the stack is as follows:
## expr COMMA
##

Ill-formed tuple expression.
At this point, a tuple component is expected as an expression.

interactive_expr: LPAR With
##
## Ends in an error in state: 130.
##
## par(expr) -> LPAR . expr RPAR [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive DOT Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
## par(tuple(expr)) -> LPAR . tuple(expr) RPAR [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
## par(typed_expr) -> LPAR . typed_expr RPAR [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR
##

Ill-formed expression.
At this point, an expression is expected, denoting either
  * an expression annotated with its type,
  * a parenthesised expression,
  * a tuple component.

interactive_expr: Lang Verbatim VBAR
##
## Ends in an error in state: 660.
##
## code_inj -> Lang expr . RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Lang expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

Ill-formed code injection.
At this point, if the expression denoting the code to inject is
complete, then a closing bracket ']' is expected.

interactive_expr: Lang With
##
## Ends in an error in state: 129.
##
## code_inj -> Lang . expr RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Lang
##

Ill-formed code injection.
At this point, the code is expected as an expression whose value is a
verbatim string.

interactive_expr: List LBRACKET Verbatim RBRACE
##
## Ends in an error in state: 664.
##
## injection(List,expr) -> List LBRACKET option(sep_or_term_list(expr,SEMI)) . RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## List LBRACKET option(sep_or_term_list(expr,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 384, spurious reduction of production nsepseq(expr,SEMI) -> expr
## In state 383, spurious reduction of production sep_or_term_list(expr,SEMI) -> nsepseq(expr,SEMI)
## In state 663, spurious reduction of production option(sep_or_term_list(expr,SEMI)) -> sep_or_term_list(expr,SEMI)
##

Ill-formed list of expressions.
At this point, if the list element is complete, one of the following
is expected:
  * a semicolon ';' followed by another element as an expression;
  * a closing bracket ']' if the list is complete.

interactive_expr: List LBRACKET With
##
## Ends in an error in state: 662.
##
## injection(List,expr) -> List LBRACKET . option(sep_or_term_list(expr,SEMI)) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## List LBRACKET
##

Ill-formed list of expressions.
At this point, one of the following is expected:
  * list elements as expressions separated by semicolons ';';
  * a closing bracket ']' if the list is empty.

interactive_expr: List Verbatim EOF
##
## Ends in an error in state: 666.
##
## injection(List,expr) -> List option(sep_or_term_list(expr,SEMI)) . End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## List option(sep_or_term_list(expr,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 384, spurious reduction of production nsepseq(expr,SEMI) -> expr
## In state 383, spurious reduction of production sep_or_term_list(expr,SEMI) -> nsepseq(expr,SEMI)
## In state 663, spurious reduction of production option(sep_or_term_list(expr,SEMI)) -> sep_or_term_list(expr,SEMI)
##

Ill-formed list of expressions.
At this point, if the list element is complete, one of the following
is expected:
  * a semicolon ';' followed by another element as an expression;
  * the keyword 'end' if the list is complete.

interactive_expr: List With
##
## Ends in an error in state: 128.
##
## injection(List,expr) -> List . LBRACKET option(sep_or_term_list(expr,SEMI)) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
## injection(List,expr) -> List . option(sep_or_term_list(expr,SEMI)) End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## List
##

Ill-formed list of expressions.
At this point, one of the following is expected:
  * the brackets '[]' or the keyword 'end' if the list is empty;
  * list elements as expressions separated by semicolons ';';
  * as the previous case, but enclosed between brackets '[' and ']'.

interactive_expr: MINUS With
##
## Ends in an error in state: 127.
##
## unary_op(MINUS,update_expr_level) -> MINUS . update_expr_level [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## MINUS
##

Ill-formed unary expression.
At this point, an expression is expected.

interactive_expr: Map LBRACKET Verbatim ARROW Bytes RBRACE
##
## Ends in an error in state: 670.
##
## injection(Map,binding) -> Map LBRACKET option(sep_or_term_list(binding,SEMI)) . RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Map LBRACKET option(sep_or_term_list(binding,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 398, spurious reduction of production binding -> expr ARROW expr
## In state 399, spurious reduction of production nsepseq(binding,SEMI) -> binding
## In state 395, spurious reduction of production sep_or_term_list(binding,SEMI) -> nsepseq(binding,SEMI)
## In state 621, spurious reduction of production option(sep_or_term_list(binding,SEMI)) -> sep_or_term_list(binding,SEMI)
##

Ill-formed map expression.
At this point, if the key/value binding is complete, one of the
following is expected:
  * a semicolon ';' followed by another key/value binding;
  * a closing bracket ']' if the map is complete.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Map LBRACKET With
##
## Ends in an error in state: 669.
##
## injection(Map,binding) -> Map LBRACKET . option(sep_or_term_list(binding,SEMI)) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Map LBRACKET
##

Ill-formed map expression.
At this point, one of the following is expected:
  * key/value bindings separated by semicolons ';';
  * a closing bracket ']' if the map is empty.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Map Verbatim ARROW With
##
## Ends in an error in state: 397.
##
## binding -> expr ARROW . expr [ SEMI RBRACKET End ]
##
## The known suffix of the stack is as follows:
## expr ARROW
##

Ill-formed key/value binding in a map.
At this point, the value is expected as an expression.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Map With
##
## Ends in an error in state: 126.
##
## injection(Map,binding) -> Map . LBRACKET option(sep_or_term_list(binding,SEMI)) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
## injection(Map,binding) -> Map . option(sep_or_term_list(binding,SEMI)) End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Map
##

Ill-formed map expression.
At this point, one of the following is expected:
  * the brackets '[]' or the keyword 'end' if the map is empty;
  * key/value bindings separated by semicolons ';';
  * as the previous case, but enclosed between brackets '[' and ']'.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Not With
##
## Ends in an error in state: 122.
##
## unary_op(Not,update_expr_level) -> Not . update_expr_level [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Not
##

Ill-formed unary expression.
At this point, an expression is expected.

interactive_expr: Ident With Record Ident EQ With
##
## Ends in an error in state: 291.
##
## field_path_assignment -> field_path EQ . expr [ SEMI RBRACKET End ]
##
## The known suffix of the stack is as follows:
## field_path EQ
##
interactive_expr: Record Ident EQ With
##
## Ends in an error in state: 121.
##
## field_assignment -> Ident EQ . expr [ SEMI RBRACKET End ]
##
## The known suffix of the stack is as follows:
## Ident EQ
##

Ill-formed record field assignment.
At this point, an expression is expected, whose value is that of the
field.

interactive_expr: Record Ident With
##
## Ends in an error in state: 684.
##
## ne_injection(Record,field_assignment) -> Record sep_or_term_list(field_assignment,SEMI) . End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Record sep_or_term_list(field_assignment,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 120, spurious reduction of production field_assignment -> Ident
## In state 680, spurious reduction of production nsepseq(field_assignment,SEMI) -> field_assignment
## In state 679, spurious reduction of production sep_or_term_list(field_assignment,SEMI) -> nsepseq(field_assignment,SEMI)
##

Ill-formed record field assignment.
At this point, if the field assignment is complete, one of the
following is expected:
  * a semicolon ';' followed by another record field assignment;
  * the keyword 'end' if the record is complete.

interactive_expr: Record LBRACKET Ident End
##
## Ends in an error in state: 676.
##
## ne_injection(Record,field_assignment) -> Record LBRACKET sep_or_term_list(field_assignment,SEMI) . RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Record LBRACKET sep_or_term_list(field_assignment,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 120, spurious reduction of production field_assignment -> Ident
## In state 680, spurious reduction of production nsepseq(field_assignment,SEMI) -> field_assignment
## In state 679, spurious reduction of production sep_or_term_list(field_assignment,SEMI) -> nsepseq(field_assignment,SEMI)
##

Ill-formed record expression.
At this point, if the field assignment is complete, one of the
following is expected:
  * a semicolon ';' followed by another record field assignment;
  * a closing bracket ']' if the record is complete.

interactive_expr: Record LBRACKET With
##
## Ends in an error in state: 119.
##
## ne_injection(Record,field_assignment) -> Record LBRACKET . sep_or_term_list(field_assignment,SEMI) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Record LBRACKET
##

Ill-formed record expression.
At this point, field assignments separated by semicolons ';' are
expected.

interactive_expr: Set LBRACKET With
##
## Ends in an error in state: 686.
##
## injection(Set,expr) -> Set LBRACKET . option(sep_or_term_list(expr,SEMI)) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Set LBRACKET
##

Ill-formed set expression.
At this point, set elements separated by semicolons ';' are expected.

interactive_expr: Set Verbatim EOF
##
## Ends in an error in state: 689.
##
## injection(Set,expr) -> Set option(sep_or_term_list(expr,SEMI)) . End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Set option(sep_or_term_list(expr,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 384, spurious reduction of production nsepseq(expr,SEMI) -> expr
## In state 383, spurious reduction of production sep_or_term_list(expr,SEMI) -> nsepseq(expr,SEMI)
## In state 663, spurious reduction of production option(sep_or_term_list(expr,SEMI)) -> sep_or_term_list(expr,SEMI)
##

Ill-formed set expression.
At this point, if the set element is complete, one of the following is
expected:
  * a semicolon ';' followed by another element as an expression;
  * the keyword 'end' if the set is complete.

interactive_expr: Set With
##
## Ends in an error in state: 117.
##
## injection(Set,expr) -> Set . LBRACKET option(sep_or_term_list(expr,SEMI)) RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
## injection(Set,expr) -> Set . option(sep_or_term_list(expr,SEMI)) End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Set
##

Ill-formed set expression.
At this point, one of the following is expected:
  * the brackets '[]' or the keyword 'end' if the set is empty;
  * set elements (expressions) separated by semicolons ';';
  * as the previous case, but enclosed between brackets '[' and ']'.

interactive_expr: Verbatim CARET With
##
## Ends in an error in state: 299.
##
## bin_op(cons_expr_level,CARET,cat_expr_level) -> cons_expr_level CARET . cat_expr_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of NE Module LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## cons_expr_level CARET
##

Ill-formed string expression.
At this point, an expression is expected.

interactive_expr: Verbatim SHARP With
##
## Ends in an error in state: 312.
##
## bin_op(add_expr_level,SHARP,cons_expr_level) -> add_expr_level SHARP . cons_expr_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of NE Module LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## add_expr_level SHARP
##

Ill-formed list expression.
At this point, an expression denoting a list is expected.

interactive_expr: Verbatim Contains With
##
## Ends in an error in state: 158.
##
## set_mem_level -> update_expr_level Contains . set_mem_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of Module LBRACE Function From End Else EOF Directive Const COMMA COLON Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## update_expr_level Contains
##

Ill-formed membership test in a set.
At this point, an expression denoting a set element is expected.

interactive_expr: Verbatim And With
##
## Ends in an error in state: 323.
##
## conj_expr_level -> conj_expr_level And . set_mem_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of Module LBRACE Function From End Else EOF Directive Const COMMA COLON Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## conj_expr_level And
##
interactive_expr: Verbatim Or With
##
## Ends in an error in state: 297.
##
## disj_expr_level -> disj_expr_level Or . conj_expr_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of Module LBRACE Function From End Else EOF Directive Const COMMA COLON Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## disj_expr_level Or
##

Ill-formed boolean expression.
At this point, a boolean expression is expected.

interactive_expr: Verbatim EQ With
##
## Ends in an error in state: 336.
##
## bin_op(comp_expr_level,EQ,cat_expr_level) -> comp_expr_level EQ . cat_expr_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of NE Module LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## comp_expr_level EQ
##
interactive_expr: Verbatim NE With
##
## Ends in an error in state: 326.
##
## bin_op(comp_expr_level,NE,cat_expr_level) -> comp_expr_level NE . cat_expr_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of NE Module LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## comp_expr_level NE
##
interactive_expr: Verbatim GE With
##
## Ends in an error in state: 334.
##
## bin_op(comp_expr_level,GE,cat_expr_level) -> comp_expr_level GE . cat_expr_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of NE Module LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## comp_expr_level GE
##
interactive_expr: Verbatim GT With
##
## Ends in an error in state: 332.
##
## bin_op(comp_expr_level,GT,cat_expr_level) -> comp_expr_level GT . cat_expr_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of NE Module LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## comp_expr_level GT
##
interactive_expr: Verbatim LE With
##
## Ends in an error in state: 330.
##
## bin_op(comp_expr_level,LE,cat_expr_level) -> comp_expr_level LE . cat_expr_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of NE Module LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## comp_expr_level LE
##
interactive_expr: Verbatim LT With
##
## Ends in an error in state: 328.
##
## bin_op(comp_expr_level,LT,cat_expr_level) -> comp_expr_level LT . cat_expr_level [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Or Of NE Module LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## comp_expr_level LT
##

Ill-formed boolean expression.
At this point, a comparable expression is expected.

interactive_expr: Verbatim MINUS With
##
## Ends in an error in state: 320.
##
## bin_op(add_expr_level,MINUS,mult_expr_level) -> add_expr_level MINUS . mult_expr_level [ VBAR Type To Then Step SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## add_expr_level MINUS
##
interactive_expr: Verbatim Mod With
##
## Ends in an error in state: 318.
##
## bin_op(mult_expr_level,Mod,unary_expr_level) -> mult_expr_level Mod . unary_expr_level [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## mult_expr_level Mod
##
interactive_expr: Verbatim PLUS With
##
## Ends in an error in state: 314.
##
## bin_op(add_expr_level,PLUS,mult_expr_level) -> add_expr_level PLUS . mult_expr_level [ VBAR Type To Then Step SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## add_expr_level PLUS
##
interactive_expr: Verbatim SLASH With
##
## Ends in an error in state: 316.
##
## bin_op(mult_expr_level,SLASH,unary_expr_level) -> mult_expr_level SLASH . unary_expr_level [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## mult_expr_level SLASH
##
interactive_expr: Verbatim TIMES With
##
## Ends in an error in state: 263.
##
## bin_op(mult_expr_level,TIMES,unary_expr_level) -> mult_expr_level TIMES . unary_expr_level [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## mult_expr_level TIMES
##

Ill-formed arithmetic expression.
At this point, an expression is expected.

interactive_expr: With
##
## Ends in an error in state: 706.
##
## interactive_expr' -> . interactive_expr [ # ]
##
## The known suffix of the stack is as follows:
##
##

Ill-formed expression.

contract: Const Ident EQ With
##
## Ends in an error in state: 427.
##
## unqualified_decl(EQ) -> core_pattern EQ . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## core_pattern EQ
##
contract: Const Ident COLON UIdent EQ With
##
## Ends in an error in state: 430.
##
## unqualified_decl(EQ) -> core_pattern type_annotation EQ . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## core_pattern type_annotation EQ
##

Ill-formed constant declaration.
At this point, an expression is expected, whose value is that of the
constant.

contract: Const Ident With
##
## Ends in an error in state: 426.
##
## unqualified_decl(EQ) -> core_pattern . EQ expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## unqualified_decl(EQ) -> core_pattern . type_annotation EQ expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## core_pattern
##

Ill-formed constant declaration.
At this point, one of the following is expected:
  * a type annotation starting with a colon ':';
  * the symbol '=' followed by an expression whose value is that of
    the constant being defined.

contract: Const With
##
## Ends in an error in state: 424.
##
## const_decl -> Const . unqualified_decl(EQ) [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Const
##
contract: Attr Const With
##
## Ends in an error in state: 452.
##
## const_decl -> nseq(Attr) Const . unqualified_decl(EQ) [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Const
##

Ill-formed constant declaration.
At this point, a pattern is expected, for instance, the name of the
constant being defined, or the wildcard pattern '_'.

contract: Function Ident LPAR Const Ident RPAR COLON Ident With
##
## Ends in an error in state: 420.
##
## fun_decl -> Function Ident parameters type_annotation . Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Function Ident parameters type_annotation
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 25, spurious reduction of production core_type -> Ident
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
##
contract: Recursive Function Ident LPAR Const Ident RPAR COLON String With
##
## Ends in an error in state: 695.
##
## fun_decl -> Recursive Function Ident parameters type_annotation . Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Recursive Function Ident parameters type_annotation
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
##
contract: Attr Recursive Function Ident LPAR Const Ident RPAR COLON UIdent With
##
## Ends in an error in state: 441.
##
## fun_decl -> nseq(Attr) Recursive Function Ident parameters type_annotation . Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Recursive Function Ident parameters type_annotation
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 15, spurious reduction of production variant -> UIdent
## In state 32, spurious reduction of production nsepseq(variant,VBAR) -> variant
## In state 62, spurious reduction of production sum_type -> nsepseq(variant,VBAR)
## In state 58, spurious reduction of production type_expr -> sum_type
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
##
contract: Attr Function Ident LPAR Const Ident RPAR COLON UIdent With
##
## Ends in an error in state: 449.
##
## fun_decl -> nseq(Attr) Function Ident parameters type_annotation . Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Function Ident parameters type_annotation
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 15, spurious reduction of production variant -> UIdent
## In state 32, spurious reduction of production nsepseq(variant,VBAR) -> variant
## In state 62, spurious reduction of production sum_type -> nsepseq(variant,VBAR)
## In state 58, spurious reduction of production type_expr -> sum_type
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
##

Ill-formed function declaration.
At this point, if the type expression of the return value is complete,
the keyword 'is' is expected, followed by the function body as an
expression.

contract: Function Ident LPAR Const Ident RPAR COLON UIdent Is With
##
## Ends in an error in state: 421.
##
## fun_decl -> Function Ident parameters type_annotation Is . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Function Ident parameters type_annotation Is
##
contract: Attr Function Ident LPAR Const Ident RPAR Is With
##
## Ends in an error in state: 447.
##
## fun_decl -> nseq(Attr) Function Ident parameters Is . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Function Ident parameters Is
##
contract: Function Ident LPAR Const Ident RPAR Is With
##
## Ends in an error in state: 418.
##
## fun_decl -> Function Ident parameters Is . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Function Ident parameters Is
##
contract: Recursive Function Ident LPAR Const Ident RPAR Is With
##
## Ends in an error in state: 110.
##
## fun_decl -> Recursive Function Ident parameters Is . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Recursive Function Ident parameters Is
##
contract: Attr Recursive Function Ident LPAR Const Ident RPAR COLON UIdent Is With
##
## Ends in an error in state: 442.
##
## fun_decl -> nseq(Attr) Recursive Function Ident parameters type_annotation Is . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Recursive Function Ident parameters type_annotation Is
##
contract: Attr Function Ident LPAR Const Ident RPAR COLON UIdent Is With
##
## Ends in an error in state: 450.
##
## fun_decl -> nseq(Attr) Function Ident parameters type_annotation Is . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Function Ident parameters type_annotation Is
##
interactive_expr: Function LPAR Const Ident RPAR COLON WILD Is With
##
## Ends in an error in state: 643.
##
## fun_expr(expr) -> Function parameters type_annotation Is . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function parameters type_annotation Is
##
contract: Recursive Function Ident LPAR Const Ident RPAR COLON WILD Is With
##
## Ends in an error in state: 696.
##
## fun_decl -> Recursive Function Ident parameters type_annotation Is . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Recursive Function Ident parameters type_annotation Is
##
contract: Attr Recursive Function Ident LPAR Const Ident RPAR Is With
##
## Ends in an error in state: 439.
##
## fun_decl -> nseq(Attr) Recursive Function Ident parameters Is . expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Recursive Function Ident parameters Is
##
interactive_expr: If Verbatim Then Function LPAR Const Ident RPAR COLON Ident Is With
##
## Ends in an error in state: 498.
##
## fun_expr(closed_expr) -> Function parameters type_annotation Is . closed_expr [ Else ]
## fun_expr(expr) -> Function parameters type_annotation Is . expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Function parameters type_annotation Is
##

Ill-formed function declaration.
At this point, the function body is expected as an expression.

contract: Function Ident LPAR Const Ident RPAR With
##
## Ends in an error in state: 417.
##
## fun_decl -> Function Ident parameters . Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> Function Ident parameters . type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Function Ident parameters
##

Ill-formed function declaration.
At this point, if the parameters are complete, one of the following is
expected:
  * a type annotation starting with a colon ':';
  * the keyword 'is' followed by the function body an expression.

contract: Function Ident With
##
## Ends in an error in state: 416.
##
## fun_decl -> Function Ident . parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> Function Ident . parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Function Ident
##
contract: Recursive Function Ident With
##
## Ends in an error in state: 91.
##
## fun_decl -> Recursive Function Ident . parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> Recursive Function Ident . parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Recursive Function Ident
##

Ill-formed function declaration.
At this point, the parameter declarations are expected between
parentheses.

contract: Function With
##
## Ends in an error in state: 415.
##
## fun_decl -> Function . Ident parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> Function . Ident parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Function
##
contract: Recursive Function With
##
## Ends in an error in state: 90.
##
## fun_decl -> Recursive Function . Ident parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> Recursive Function . Ident parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Recursive Function
##
contract: Attr Function With
##
## Ends in an error in state: 444.
##
## fun_decl -> nseq(Attr) Function . Ident parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) Function . Ident parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Function
##
contract: Attr Recursive Function With
##
## Ends in an error in state: 436.
##
## fun_decl -> nseq(Attr) Recursive Function . Ident parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) Recursive Function . Ident parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Recursive Function
##

Ill-formed function declaration.
At this point, the name of the function is expected.

contract: Recursive Function Ident LPAR Const Ident RPAR With
##
## Ends in an error in state: 109.
##
## fun_decl -> Recursive Function Ident parameters . Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> Recursive Function Ident parameters . type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Recursive Function Ident parameters
##

Ill-formed function declaration.
At this point, if the paramters are complete, one of the following is
expected:
  * a type annotation for the result, starting with a colon ':';
  * the keyword 'is' followed by the function body as an expression.

contract: Recursive With
##
## Ends in an error in state: 89.
##
## fun_decl -> Recursive . Function Ident parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> Recursive . Function Ident parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Recursive
##
contract: Attr Recursive With
##
## Ends in an error in state: 435.
##
## fun_decl -> nseq(Attr) Recursive . Function Ident parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) Recursive . Function Ident parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Recursive
##

Ill-formed function declaration.
At this point, the keyword 'function' is expected, followed by the name
of the function.

contract: Type Ident Is BigMap With
##
## Ends in an error in state: 28.
##
## type_ctor_app -> BigMap . type_tuple [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## BigMap
##

Ill-formed big_map type.
At this point, a pair of types is expected: the type of the keys and
the type of the values.

contract: Type Ident Is Map With
##
## Ends in an error in state: 20.
##
## type_ctor_app -> Map . type_tuple [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Map
##

Ill-formed map type.
At this point, a pair of types is expected: the type of the keys and
the type of the values.

contract: Type Ident Is UIdent DOT With
##
## Ends in an error in state: 9.
##
## module_path(type_name) -> UIdent DOT . module_path(type_name) [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
## module_path(type_name) -> UIdent DOT . Ident [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## UIdent DOT
##

Ill-formed selection of a type in a module.
At this point, the qualified name of a type is expected.

contract: Type Ident Is UIdent Of With
##
## Ends in an error in state: 7.
##
## variant -> UIdent Of . fun_type_level [ VBAR Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
##
## The known suffix of the stack is as follows:
## UIdent Of
##
contract: Type Ident Is Attr UIdent Of With
##
## Ends in an error in state: 36.
##
## variant -> nseq(Attr) UIdent Of . fun_type_level [ VBAR Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) UIdent Of
##

Ill-formed variant definition.
At this point, a type that is neither a record nor a sum type is
expected as the parameter of the constructor.

contract: Type Ident Is UIdent VBAR With
##
## Ends in an error in state: 33.
##
## nsepseq(variant,VBAR) -> variant VBAR . nsepseq(variant,VBAR) [ Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
##
## The known suffix of the stack is as follows:
## variant VBAR
##
contract: Type Ident Is VBAR With
##
## Ends in an error in state: 5.
##
## sum_type -> VBAR . nsepseq(variant,VBAR) [ Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
##
## The known suffix of the stack is as follows:
## VBAR
##

Ill-formed variant type.
At this point, a variant is expected, starting with a data constructor.

contract: Type Ident Is WILD TIMES UIdent With
##
## Ends in an error in state: 8.
##
## module_path(type_name) -> UIdent . DOT module_path(type_name) [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
## module_path(type_name) -> UIdent . DOT Ident [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## UIdent
##

Ill-formed selection of a type in a module.
At this point, the selection operator '.' is expected, followed by the
qualified name of a type.

contract: Type Ident Is With
##
## Ends in an error in state: 3.
##
## type_decl -> Type Ident Is . type_expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Type Ident Is
##

Ill-formed type declaration.
At this point, a type expression is expected.

contract: Type Ident Is LPAR With
##
## Ends in an error in state: 14.
##
## par(type_expr) -> LPAR . type_expr RPAR [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR
##

At this point, a type expression is expected, follwed by a closing
parenthesis ')'.

contract: Type Ident Is LPAR String With
##
## Ends in an error in state: 83.
##
## par(type_expr) -> LPAR type_expr . RPAR [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR type_expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
##

Ill-formed parenthesised type expression.
At this point, if the type expression is complete, a closing
parenthesis ')' is expected.

contract: Type Ident Is List With
##
## Ends in an error in state: 22.
##
## type_ctor_app -> List . par(type_expr) [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## List
##

Ill-formed list type.
At this point, a parenthesised type is expected, which is the type of
the list elements.

contract: Type Ident Is Map LPAR String With
##
## Ends in an error in state: 65.
##
## par(nsepseq(type_expr,COMMA)) -> LPAR nsepseq(type_expr,COMMA) . RPAR [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR nsepseq(type_expr,COMMA)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
## In state 56, spurious reduction of production nsepseq(type_expr,COMMA) -> type_expr
##

Ill-formed tuple of types.
At this point, if the type expression is complete, one of the
following is expected:
  * a comma ',' followed by another type expression (component);
  * a closing parenthesis ')' if the type tuple is complete.

contract: Type Ident Is Record Ident With
##
## Ends in an error in state: 81.
##
## ne_injection(Record,field_decl) -> Record sep_or_term_list(field_decl,SEMI) . End [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Record sep_or_term_list(field_decl,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 18, spurious reduction of production field_decl -> Ident
## In state 77, spurious reduction of production nsepseq(field_decl,SEMI) -> field_decl
## In state 76, spurious reduction of production sep_or_term_list(field_decl,SEMI) -> nsepseq(field_decl,SEMI)
##

Ill-formed record type.
At this point, if the field declaration is complete, one of the
following is expected:
  * a semicolon ';' followed by another field declaration;
  * the keyword 'end' if the record type is complete.

contract: Type Ident Is Record Ident COLON With
##
## Ends in an error in state: 19.
##
## type_annotation -> COLON . type_expr [ SEMI RPAR RBRACKET Is End EQ ASSIGN ]
##
## The known suffix of the stack is as follows:
## COLON
##

Ill-formed type annotation.
At this point, a type expression is expected.

contract: Type Ident Is Record LBRACKET Ident COLON Ident With
##
## Ends in an error in state: 70.
##
## ne_injection(Record,field_decl) -> Record LBRACKET sep_or_term_list(field_decl,SEMI) . RBRACKET [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Record LBRACKET sep_or_term_list(field_decl,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 25, spurious reduction of production core_type -> Ident
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
## In state 69, spurious reduction of production field_decl -> Ident type_annotation
## In state 77, spurious reduction of production nsepseq(field_decl,SEMI) -> field_decl
## In state 76, spurious reduction of production sep_or_term_list(field_decl,SEMI) -> nsepseq(field_decl,SEMI)
##

Ill-formed record type.
At this point, if the field declaration is complete, one of the
following is expected:
  * a semicolon ';' followed by another field declaration;
  * a closing bracket ']' if the record type is complete.

contract: Type Ident Is Record LBRACKET With
##
## Ends in an error in state: 17.
##
## ne_injection(Record,field_decl) -> Record LBRACKET . sep_or_term_list(field_decl,SEMI) RBRACKET [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Record LBRACKET
##

Ill-formed record type.
At this point, field declarations are expected, separated by
semicolons ';'.

contract: Type Ident Is Set With
##
## Ends in an error in state: 13.
##
## type_ctor_app -> Set . par(type_expr) [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Set
##

Ill-formed set type.
At this point, the type of the set elements is expected between
parentheses.

contract: Type Ident Is String ARROW With
##
## Ends in an error in state: 53.
##
## fun_type_level -> cartesian_level ARROW . fun_type_level [ VBAR Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
##
## The known suffix of the stack is as follows:
## cartesian_level ARROW
##

Ill-formed function type.
At this point, a type expression is expected.

contract: Type Ident Is String TIMES String TIMES With
##
## Ends in an error in state: 50.
##
## nsepseq(core_type,TIMES) -> core_type TIMES . nsepseq(core_type,TIMES) [ VBAR Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## core_type TIMES
##
contract: Type Ident Is String TIMES With
##
## Ends in an error in state: 47.
##
## cartesian_level -> core_type TIMES . nsepseq(core_type,TIMES) [ VBAR Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## core_type TIMES
##

Ill-formed product of types.
At this point, a type expression is expected.

contract: Type Ident With
##
## Ends in an error in state: 2.
##
## type_decl -> Type Ident . Is type_expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Type Ident
##

Ill-formed type declaration.
At this point, the keyword 'is' is expected, followed by a type
expression.

contract: Type With
##
## Ends in an error in state: 1.
##
## type_decl -> Type . Ident Is type_expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Type
##

Ill-formed type declaration.
At this point, the name of the type is expected.

contract: Type Ident Is VBAR Attr With
##
## Ends in an error in state: 34.
##
## variant -> nseq(Attr) . UIdent [ VBAR Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
## variant -> nseq(Attr) . UIdent Of fun_type_level [ VBAR Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
##
## The known suffix of the stack is as follows:
## nseq(Attr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 30, spurious reduction of production nseq(Attr) -> Attr
##

Ill-formed variant.
At this point, one of the following is expected:
  * the data constructor of the variant;
  * another attribute for the variant.

contract: Const Ident COLON UIdent With
##
## Ends in an error in state: 429.
##
## unqualified_decl(EQ) -> core_pattern type_annotation . EQ expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## core_pattern type_annotation
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 15, spurious reduction of production variant -> UIdent
## In state 32, spurious reduction of production nsepseq(variant,VBAR) -> variant
## In state 62, spurious reduction of production sum_type -> nsepseq(variant,VBAR)
## In state 58, spurious reduction of production type_expr -> sum_type
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
##

Ill-formed constant declaration.
At this point, if the type annotation is complete, the symbol '=' is
expected, followed by an expression.

interactive_expr: Map Verbatim ARROW Bytes SEMI With
##
## Ends in an error in state: 672.
##
## injection(Map,binding) -> Map option(sep_or_term_list(binding,SEMI)) . End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Map option(sep_or_term_list(binding,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 400, spurious reduction of production nseq(__anonymous_0(binding,SEMI)) -> binding SEMI
## In state 394, spurious reduction of production sep_or_term_list(binding,SEMI) -> nseq(__anonymous_0(binding,SEMI))
## In state 621, spurious reduction of production option(sep_or_term_list(binding,SEMI)) -> sep_or_term_list(binding,SEMI)
##

Ill-formed map expression.
At this point, if the key/value binding is complete, one of the
following is expected:
  * a semicolon ';' followed by another key/value binding;
  * the keyword 'end' if the map is complete.
Note: A binding is made of two expressions (a key and a value)
separated by an arrow '->'.

interactive_expr: Case Verbatim Of List WILD SEMI With
##
## Ends in an error in state: 243.
##
## injection(List,core_pattern) -> List option(sep_or_term_list(core_pattern,SEMI)) . End [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## List option(sep_or_term_list(core_pattern,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 240, spurious reduction of production nseq(__anonymous_0(core_pattern,SEMI)) -> core_pattern SEMI
## In state 237, spurious reduction of production sep_or_term_list(core_pattern,SEMI) -> nseq(__anonymous_0(core_pattern,SEMI))
## In state 234, spurious reduction of production option(sep_or_term_list(core_pattern,SEMI)) -> sep_or_term_list(core_pattern,SEMI)
##

Ill-formed list of patterns.
At this point, if pattern matching a list element is complete, one of
the following is expected:
  * a semicolon ';' followed by another pattern;
  * the keyword 'end' if the list is complete.

contract: With
##
## Ends in an error in state: 0.
##
## contract' -> . contract [ # ]
##
## The known suffix of the stack is as follows:
##
##

Ill-formed contract.

contract: Type Ident Is Record With
##
## Ends in an error in state: 16.
##
## ne_injection(Record,field_decl) -> Record . LBRACKET sep_or_term_list(field_decl,SEMI) RBRACKET [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
## ne_injection(Record,field_decl) -> Record . sep_or_term_list(field_decl,SEMI) End [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Record
##

Ill-formed record type.
At this point, one of the following is expected:
  * field declarations are expected, separated by semicolons ';';
  * an opening bracket '[' followed by field declarations.
Note: A field declaration is made of a field name, optionnally
followed by a type annotation.

contract: Type Ident Is Map LPAR With
##
## Ends in an error in state: 21.
##
## par(nsepseq(type_expr,COMMA)) -> LPAR . nsepseq(type_expr,COMMA) RPAR [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR
##

Ill-formed type parameter.
At this point, components as type expressions are expected, separated
by commas ',' and concluded with a closing parenthesis ')'.

contract: Type Ident Is Map LPAR WILD COMMA With
##
## Ends in an error in state: 57.
##
## nsepseq(type_expr,COMMA) -> type_expr COMMA . nsepseq(type_expr,COMMA) [ RPAR ]
##
## The known suffix of the stack is as follows:
## type_expr COMMA
##

Ill-formed type parameter.
At this point, another component is expected as a type expression.

contract: Type Ident Is Attr Recursive
##
## Ends in an error in state: 59.
##
## record_type -> nseq(Attr) . ne_injection(Record,field_decl) [ Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
## sum_type -> nseq(Attr) . VBAR nsepseq(variant,VBAR) [ Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
## variant -> nseq(Attr) . UIdent [ VBAR Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
## variant -> nseq(Attr) . UIdent Of fun_type_level [ VBAR Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
##
## The known suffix of the stack is as follows:
## nseq(Attr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 30, spurious reduction of production nseq(Attr) -> Attr
##

Ill-formed type.
At this point, if the attribute is complete, one of the following is
expected:
  * more attributes;
  * a record type declaration starting with the keyword 'record';
  * a variant starting with a constructor.

contract: Type Ident Is Attr VBAR With
##
## Ends in an error in state: 60.
##
## sum_type -> nseq(Attr) VBAR . nsepseq(variant,VBAR) [ Type SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) VBAR
##

Ill-formed sum type.
At this point, one of the following is expected:
  * attributes of the variant;
  * the constructor name of the variant.

contract: Type Ident Is Record Attr VBAR
##
## Ends in an error in state: 73.
##
## field_decl -> nseq(Attr) . Ident [ SEMI RBRACKET End ]
## field_decl -> nseq(Attr) . Ident type_annotation [ SEMI RBRACKET End ]
##
## The known suffix of the stack is as follows:
## nseq(Attr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 30, spurious reduction of production nseq(Attr) -> Attr
##

Ill-formed record declaration.
At this point, if the attribute is complete, one of the following is
expected:
  * more attributes of the record field;
  * the name of the record field.

interactive_expr: Block LBRACE With
##
## Ends in an error in state: 178.
##
## block -> option(Block) LBRACE . sep_or_term_list(statement,SEMI) RBRACE [ With VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## option(Block) LBRACE
##
interactive_expr: Begin With
##
## Ends in an error in state: 154.
##
## block -> Begin . sep_or_term_list(statement,SEMI) End [ With VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Begin
##

Ill-formed block.
At this point, statements are expected, separated by semicolons ';'.
Note: A statement is either an instruction of a declaration.

contract: Attr VBAR
##
## Ends in an error in state: 434.
##
## const_decl -> nseq(Attr) . Const unqualified_decl(EQ) [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) . Function Ident parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) . Function Ident parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) . Recursive Function Ident parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) . Recursive Function Ident parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 30, spurious reduction of production nseq(Attr) -> Attr
##

Ill-formed declaration.
At this point, one of the following is expected:
  * more attributes;
  * the keyword 'const' if defining a constant;
  * the keyword 'function' if defining a function;
  * the keywords 'recursive function' if defining a recursive
    function.

contract: Attr Recursive Function Ident With
##
## Ends in an error in state: 437.
##
## fun_decl -> nseq(Attr) Recursive Function Ident . parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) Recursive Function Ident . parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Recursive Function Ident
##
contract: Attr Function Ident With
##
## Ends in an error in state: 445.
##
## fun_decl -> nseq(Attr) Function Ident . parameters Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) Function Ident . parameters type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Function Ident
##

Ill-formed function declaration.
At this point, the parameters are expected, between parentheses and
separated by semicolons ';'.

contract: Attr Recursive Function Ident LPAR Const Ident RPAR With
##
## Ends in an error in state: 438.
##
## fun_decl -> nseq(Attr) Recursive Function Ident parameters . Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## fun_decl -> nseq(Attr) Recursive Function Ident parameters . type_annotation Is expr [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## nseq(Attr) Recursive Function Ident parameters
##

Ill-formed function declaration.
At this point, one of the following is expected:
  * the keyword 'is' followed by an expression whose value is returned;
  * a type annotation for the return value, starting with a colon ':'.

contract: Type Ident Is UIdent With
##
## Ends in an error in state: 703.
##
## contract -> nseq(top_declaration) . EOF [ # ]
##
## The known suffix of the stack is as follows:
## nseq(top_declaration)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 15, spurious reduction of production variant -> UIdent
## In state 32, spurious reduction of production nsepseq(variant,VBAR) -> variant
## In state 62, spurious reduction of production sum_type -> nsepseq(variant,VBAR)
## In state 58, spurious reduction of production type_expr -> sum_type
## In state 88, spurious reduction of production type_decl -> Type Ident Is type_expr
## In state 432, spurious reduction of production declaration -> type_decl
## In state 701, spurious reduction of production option(SEMI) ->
## In state 702, spurious reduction of production top_declaration -> declaration option(SEMI)
## In state 699, spurious reduction of production nseq(top_declaration) -> top_declaration
##

Ill-formed contract.
At this point, one the following is expected:
  * another declaration;
  * the end of the file.

interactive_expr: Ident While
##
## Ends in an error in state: 708.
##
## interactive_expr -> expr . EOF [ # ]
##
## The known suffix of the stack is as follows:
## expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 133, spurious reduction of production field_path -> Ident
## In state 275, spurious reduction of production local_path -> field_path
## In state 269, spurious reduction of production path_expr -> local_path
## In state 168, spurious reduction of production core_expr -> path_expr
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

Ill-formed expression.

interactive_expr: Begin Var Ident COLON WILD ASSIGN With
##
## Ends in an error in state: 357.
##
## unqualified_decl(ASSIGN) -> core_pattern type_annotation ASSIGN . expr [ SEMI RBRACE End ]
##
## The known suffix of the stack is as follows:
## core_pattern type_annotation ASSIGN
##

Ill-formed variable declaration.
At this point, the initial value of the variable is expected as an
expression.

contract: Module UIdent Is Begin Type Ident Is UIdent RBRACE
##
## Ends in an error in state: 457.
##
## module_decl -> Module UIdent Is Begin declarations . End [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Module UIdent Is Begin declarations
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 15, spurious reduction of production variant -> UIdent
## In state 32, spurious reduction of production nsepseq(variant,VBAR) -> variant
## In state 62, spurious reduction of production sum_type -> nsepseq(variant,VBAR)
## In state 58, spurious reduction of production type_expr -> sum_type
## In state 88, spurious reduction of production type_decl -> Type Ident Is type_expr
## In state 432, spurious reduction of production declaration -> type_decl
## In state 459, spurious reduction of production option(SEMI) ->
## In state 461, spurious reduction of production nseq(__anonymous_1) -> declaration option(SEMI)
## In state 433, spurious reduction of production declarations -> nseq(__anonymous_1)
##

Ill-formed module declaration.
At this point, if the declarations in the module are complete, the
keyword 'end' is expected.

contract: Module UIdent Is Begin With
##
## Ends in an error in state: 414.
##
## module_decl -> Module UIdent Is Begin . declarations End [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Module UIdent Is Begin
##
contract: Module UIdent Is LBRACE With
##
## Ends in an error in state: 465.
##
## module_decl -> Module UIdent Is option(Block) LBRACE . declarations RBRACE [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Module UIdent Is option(Block) LBRACE
##

Ill-formed module declaration.
At this point, one of the following is expected:
  * a type declaration starting with the keyword 'type';
  * a value declaration starting with the keyword 'const' or 'var';
  * a (sub)module declaration starting with the keyword 'module'.

contract: Module UIdent Is LBRACE Type Ident Is UIdent End
##
## Ends in an error in state: 466.
##
## module_decl -> Module UIdent Is option(Block) LBRACE declarations . RBRACE [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Module UIdent Is option(Block) LBRACE declarations
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 15, spurious reduction of production variant -> UIdent
## In state 32, spurious reduction of production nsepseq(variant,VBAR) -> variant
## In state 62, spurious reduction of production sum_type -> nsepseq(variant,VBAR)
## In state 58, spurious reduction of production type_expr -> sum_type
## In state 88, spurious reduction of production type_decl -> Type Ident Is type_expr
## In state 432, spurious reduction of production declaration -> type_decl
## In state 459, spurious reduction of production option(SEMI) ->
## In state 461, spurious reduction of production nseq(__anonymous_1) -> declaration option(SEMI)
## In state 433, spurious reduction of production declarations -> nseq(__anonymous_1)
##

Ill-formed module declaration.
At this point, if the declarations in the module are complete, a
closing brace '}' is expected to end the module declaration.

contract: Module UIdent Is With
##
## Ends in an error in state: 410.
##
## module_alias -> Module UIdent Is . nsepseq(module_name,DOT) [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## module_decl -> Module UIdent Is . option(Block) LBRACE declarations RBRACE [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## module_decl -> Module UIdent Is . Begin declarations End [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Module UIdent Is
##

Ill-formed module declaration.
At this point, one of the following is expected:
  * the qualified name of a module being renamed;
  * a module structure, either starting with an opening brace '{' or
    the keyword 'begin'.

contract: Module UIdent With
##
## Ends in an error in state: 409.
##
## module_alias -> Module UIdent . Is nsepseq(module_name,DOT) [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## module_decl -> Module UIdent . Is option(Block) LBRACE declarations RBRACE [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## module_decl -> Module UIdent . Is Begin declarations End [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Module UIdent
##

Ill-formed module declaration.
At this point, the keyword 'is' is expected to introduce either the
qualified name of a module being renamed, or a module structure.

contract: Module With
##
## Ends in an error in state: 408.
##
## module_alias -> Module . UIdent Is nsepseq(module_name,DOT) [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## module_decl -> Module . UIdent Is option(Block) LBRACE declarations RBRACE [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
## module_decl -> Module . UIdent Is Begin declarations End [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Module
##

Ill-formed module declaration.
At this point, the name of the module being declared or renamed is
expected.

contract: Module UIdent Is UIdent DOT With
##
## Ends in an error in state: 412.
##
## nsepseq(module_name,DOT) -> UIdent DOT . nsepseq(module_name,DOT) [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## UIdent DOT
##

Ill-formed module qualification.
At this point, a qualified module name is expected.

interactive_expr: Function LPAR Var WILD With
##
## Ends in an error in state: 94.
##
## param_decl -> Var WILD . option(type_annotation) [ SEMI RPAR ]
##
## The known suffix of the stack is as follows:
## Var WILD
##
interactive_expr: Function LPAR Const WILD With
##
## Ends in an error in state: 100.
##
## param_decl -> Const WILD . option(type_annotation) [ SEMI RPAR ]
##
## The known suffix of the stack is as follows:
## Const WILD
##

Ill-formed function parameter declaration.
At this point, one of the following is expected:
  * a type annotation starting with a colon ':';
  * a semicolon ';' followed with another parameter declaration;
  * a closing parenthesis ')' if no more parameters.

contract: Module UIdent Is Block With
##
## Ends in an error in state: 464.
##
## module_decl -> Module UIdent Is option(Block) . LBRACE declarations RBRACE [ Type SEMI Recursive RBRACE Module Function End EOF Directive Const Attr ]
##
## The known suffix of the stack is as follows:
## Module UIdent Is option(Block)
##

Ill-formed local module declaration.
At this point, an opening brace '{' is expected, followed by
declarations.

interactive_expr: Case Verbatim Of LPAR WILD COMMA WILD With
##
## Ends in an error in state: 222.
##
## nsepseq(pattern,COMMA) -> pattern . [ RPAR ]
## nsepseq(pattern,COMMA) -> pattern . COMMA nsepseq(pattern,COMMA) [ RPAR ]
##
## The known suffix of the stack is as follows:
## pattern
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 225, spurious reduction of production pattern -> core_pattern
##

Ill-formed tuple of patterns.
At this point, if the pattern is complete, one of the following is
expected:
  * a closing parenthesis ')' if the tuple is complete;
  * a comma ',' followed by another component as a pattern.

interactive_expr: Case Verbatim Of LPAR WILD COMMA WILD COMMA With
##
## Ends in an error in state: 223.
##
## nsepseq(pattern,COMMA) -> pattern COMMA . nsepseq(pattern,COMMA) [ RPAR ]
##
## The known suffix of the stack is as follows:
## pattern COMMA
##

Ill-formed tuple of patterns.
At this point, another component is expected as a pattern.

interactive_expr: Case Verbatim Of UIdent LPAR WILD With
##
## Ends in an error in state: 256.
##
## tuple(pattern) -> pattern . COMMA nsepseq(pattern,COMMA) [ RPAR ]
##
## The known suffix of the stack is as follows:
## pattern
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 225, spurious reduction of production pattern -> core_pattern
##

Ill-formed tuple of patterns.
At this point, if the pattern is complete, a comma ',' is expected,
followed by another component as a pattern.

contract: Const Record With
##
## Ends in an error in state: 186.
##
## ne_injection(Record,field_pattern) -> Record . LBRACKET sep_or_term_list(field_pattern,SEMI) RBRACKET [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
## ne_injection(Record,field_pattern) -> Record . sep_or_term_list(field_pattern,SEMI) End [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Record
##

Ill-formed record pattern.
At this point, one of the following is expected:
  * patterns of assignments to record fields, separated by semicolons
    ';' and followed by the keyword 'end';
  * an opening bracket '[' followed by patterns of assignments and a
    closing bracket ']'.

contract: Const Record LBRACKET With
##
## Ends in an error in state: 187.
##
## ne_injection(Record,field_pattern) -> Record LBRACKET . sep_or_term_list(field_pattern,SEMI) RBRACKET [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Record LBRACKET
##

Ill-formed record pattern.
At this point, patterns of field assignments separated by semicolons
';' are expected.

contract: Const Record Ident EQ With
##
## Ends in an error in state: 189.
##
## field_pattern -> Ident EQ . core_pattern [ SEMI RBRACKET End ]
##
## The known suffix of the stack is as follows:
## Ident EQ
##

Ill-formed pattern of a record field assignment.
At this point, a pattern matching an expression is expected, whose
value is that of the field.

contract: Const Record LBRACKET Ident End
##
## Ends in an error in state: 246.
##
## ne_injection(Record,field_pattern) -> Record LBRACKET sep_or_term_list(field_pattern,SEMI) . RBRACKET [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Record LBRACKET sep_or_term_list(field_pattern,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 188, spurious reduction of production field_pattern -> Ident
## In state 250, spurious reduction of production nsepseq(field_pattern,SEMI) -> field_pattern
## In state 249, spurious reduction of production sep_or_term_list(field_pattern,SEMI) -> nsepseq(field_pattern,SEMI)
##

Ill-formed record pattern.
At this point, if the pattern of the field assignment is complete, one
of the following is expected:
  * a semicolon ';' followed by another pattern of field assignment;
  * a closing bracket ']' if the record pattern is complete.

contract: Const Record Ident With
##
## Ends in an error in state: 254.
##
## ne_injection(Record,field_pattern) -> Record sep_or_term_list(field_pattern,SEMI) . End [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## Record sep_or_term_list(field_pattern,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 188, spurious reduction of production field_pattern -> Ident
## In state 250, spurious reduction of production nsepseq(field_pattern,SEMI) -> field_pattern
## In state 249, spurious reduction of production sep_or_term_list(field_pattern,SEMI) -> nsepseq(field_pattern,SEMI)
##

Ill-formed record pattern.
At this point, if the pattern of field assignment is complete, one of
the following is expected:
  * a semicolon ';' followed by another pattern of field assignment;
  * a keyword 'end' if the record pattern is complete.

contract: Const LPAR WILD COLON WILD With
##
## Ends in an error in state: 215.
##
## par(typed_pattern) -> LPAR typed_pattern . RPAR [ SHARP SEMI RPAR RBRACKET End EQ COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LPAR typed_pattern
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 46, spurious reduction of production cartesian_level -> core_type
## In state 52, spurious reduction of production fun_type_level -> cartesian_level
## In state 64, spurious reduction of production type_expr -> fun_type_level
## In state 68, spurious reduction of production type_annotation -> COLON type_expr
## In state 232, spurious reduction of production typed_pattern -> pattern type_annotation
##

Ill-formed typed pattern.
At this point, if the type annotation is complete, a closing
parenthesis ')' is expected.

interactive_expr: Verbatim With Record LBRACKET Ident EQ Bytes While
##
## Ends in an error in state: 282.
##
## ne_injection(Record,field_path_assignment) -> Record LBRACKET sep_or_term_list(field_path_assignment,SEMI) . RBRACKET [ VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Record LBRACKET sep_or_term_list(field_path_assignment,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 295, spurious reduction of production field_path_assignment -> field_path EQ expr
## In state 286, spurious reduction of production nsepseq(field_path_assignment,SEMI) -> field_path_assignment
## In state 285, spurious reduction of production sep_or_term_list(field_path_assignment,SEMI) -> nsepseq(field_path_assignment,SEMI)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: UIdent DOT UIdent With
##
## Ends in an error in state: 361.
##
## module_path(field_path) -> UIdent . DOT module_path(field_path) [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
## module_path(field_path) -> UIdent . DOT field_path [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## UIdent
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: UIdent DOT UIdent DOT With
##
## Ends in an error in state: 362.
##
## module_path(field_path) -> UIdent DOT . module_path(field_path) [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
## module_path(field_path) -> UIdent DOT . field_path [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## UIdent DOT
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin Remove Verbatim While
##
## Ends in an error in state: 369.
##
## map_remove(expr) -> Remove expr . From Map expr [ VBAR SEMI RBRACKET RBRACE End ]
## set_remove(expr) -> Remove expr . From Set expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin Patch Verbatim With Set LBRACKET Verbatim While
##
## Ends in an error in state: 380.
##
## ne_injection(Set,expr) -> Set LBRACKET sep_or_term_list(expr,SEMI) . RBRACKET [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Set LBRACKET sep_or_term_list(expr,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 384, spurious reduction of production nsepseq(expr,SEMI) -> expr
## In state 383, spurious reduction of production sep_or_term_list(expr,SEMI) -> nsepseq(expr,SEMI)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin Patch Verbatim With Set Verbatim While
##
## Ends in an error in state: 388.
##
## ne_injection(Set,expr) -> Set sep_or_term_list(expr,SEMI) . End [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Set sep_or_term_list(expr,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 384, spurious reduction of production nsepseq(expr,SEMI) -> expr
## In state 383, spurious reduction of production sep_or_term_list(expr,SEMI) -> nsepseq(expr,SEMI)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Map Verbatim While
##
## Ends in an error in state: 396.
##
## binding -> expr . ARROW expr [ SEMI RBRACKET End ]
##
## The known suffix of the stack is as follows:
## expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin Patch Verbatim With Map Verbatim ARROW Bytes While
##
## Ends in an error in state: 403.
##
## ne_injection(Map,binding) -> Map sep_or_term_list(binding,SEMI) . End [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## Map sep_or_term_list(binding,SEMI)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 398, spurious reduction of production binding -> expr ARROW expr
## In state 399, spurious reduction of production nsepseq(binding,SEMI) -> binding
## In state 395, spurious reduction of production sep_or_term_list(binding,SEMI) -> nsepseq(binding,SEMI)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin LPAR With
##
## Ends in an error in state: 469.
##
## par(expr) -> LPAR . expr RPAR [ LBRACKET DOT ]
##
## The known suffix of the stack is as follows:
## LPAR
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin LPAR Verbatim While
##
## Ends in an error in state: 470.
##
## par(expr) -> LPAR expr . RPAR [ LBRACKET DOT ]
##
## The known suffix of the stack is as follows:
## LPAR expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin If Verbatim Then Remove With
##
## Ends in an error in state: 476.
##
## map_remove(closed_expr) -> Remove . expr From Map closed_expr [ Else ]
## map_remove(expr) -> Remove . expr From Map expr [ VBAR SEMI RBRACKET RBRACE End ]
## set_remove(closed_expr) -> Remove . expr From Set closed_expr [ Else ]
## set_remove(expr) -> Remove . expr From Set expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin If Verbatim Then Remove Verbatim While
##
## Ends in an error in state: 477.
##
## map_remove(closed_expr) -> Remove expr . From Map closed_expr [ Else ]
## map_remove(expr) -> Remove expr . From Map expr [ VBAR SEMI RBRACKET RBRACE End ]
## set_remove(closed_expr) -> Remove expr . From Set closed_expr [ Else ]
## set_remove(expr) -> Remove expr . From Set expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin If Verbatim Then Remove Verbatim From With
##
## Ends in an error in state: 478.
##
## map_remove(closed_expr) -> Remove expr From . Map closed_expr [ Else ]
## map_remove(expr) -> Remove expr From . Map expr [ VBAR SEMI RBRACKET RBRACE End ]
## set_remove(closed_expr) -> Remove expr From . Set closed_expr [ Else ]
## set_remove(expr) -> Remove expr From . Set expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove expr From
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin If Verbatim Then Remove Verbatim From Set With
##
## Ends in an error in state: 479.
##
## set_remove(closed_expr) -> Remove expr From Set . closed_expr [ Else ]
## set_remove(expr) -> Remove expr From Set . expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove expr From Set
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin If Verbatim Then Remove Verbatim From Map With
##
## Ends in an error in state: 507.
##
## map_remove(closed_expr) -> Remove expr From Map . closed_expr [ Else ]
## map_remove(expr) -> Remove expr From Map . expr [ VBAR SEMI RBRACKET RBRACE End ]
##
## The known suffix of the stack is as follows:
## Remove expr From Map
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin For Ident ASSIGN Verbatim To Verbatim While
##
## Ends in an error in state: 523.
##
## for_int -> For Ident ASSIGN expr To expr . block [ VBAR SEMI RBRACKET RBRACE End Else ]
## for_int -> For Ident ASSIGN expr To expr . step_clause block [ VBAR SEMI RBRACKET RBRACE End Else ]
##
## The known suffix of the stack is as follows:
## For Ident ASSIGN expr To expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Begin LPAR Verbatim RPAR With
##
## Ends in an error in state: 551.
##
## local_path -> par(expr) . DOT nsepseq(selection,DOT) [ LPAR LBRACKET ASSIGN ]
## map_lookup -> par(expr) . brackets(expr) [ ASSIGN ]
##
## The known suffix of the stack is as follows:
## par(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Ident LBRACKET Verbatim While
##
## Ends in an error in state: 615.
##
## brackets(expr) -> LBRACKET expr . RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## LBRACKET expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: BigMap LBRACKET Verbatim ARROW Bytes RBRACE
##
## Ends in an error in state: 622.
##
## injection(BigMap,binding) -> BigMap LBRACKET option(sep_or_term_list(binding,SEMI)) . RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## BigMap LBRACKET option(sep_or_term_list(binding,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 398, spurious reduction of production binding -> expr ARROW expr
## In state 399, spurious reduction of production nsepseq(binding,SEMI) -> binding
## In state 395, spurious reduction of production sep_or_term_list(binding,SEMI) -> nsepseq(binding,SEMI)
## In state 621, spurious reduction of production option(sep_or_term_list(binding,SEMI)) -> sep_or_term_list(binding,SEMI)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: BigMap Verbatim ARROW Bytes Else
##
## Ends in an error in state: 624.
##
## injection(BigMap,binding) -> BigMap option(sep_or_term_list(binding,SEMI)) . End [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## BigMap option(sep_or_term_list(binding,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 398, spurious reduction of production binding -> expr ARROW expr
## In state 399, spurious reduction of production nsepseq(binding,SEMI) -> binding
## In state 395, spurious reduction of production sep_or_term_list(binding,SEMI) -> nsepseq(binding,SEMI)
## In state 621, spurious reduction of production option(sep_or_term_list(binding,SEMI)) -> sep_or_term_list(binding,SEMI)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Case Verbatim While
##
## Ends in an error in state: 626.
##
## case(expr) -> Case expr . Of LBRACKET option(VBAR) cases(expr) RBRACKET [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End Else EOF Directive Const COMMA Block Begin Attr ARROW ]
## case(expr) -> Case expr . Of option(VBAR) cases(expr) End [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End Else EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Case expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Case Verbatim Of WILD ARROW Bytes While
##
## Ends in an error in state: 640.
##
## case(expr) -> Case expr Of option(VBAR) cases(expr) . End [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End Else EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## Case expr Of option(VBAR) cases(expr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 632, spurious reduction of production case_clause(expr) -> pattern ARROW expr
## In state 636, spurious reduction of production nsepseq(case_clause(expr),VBAR) -> case_clause(expr)
## In state 633, spurious reduction of production cases(expr) -> nsepseq(case_clause(expr),VBAR)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: If Verbatim While
##
## Ends in an error in state: 644.
##
## if_then_else_expr(expr) -> If expr . Then closed_expr Else expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
## if_then_expr(expr) -> If expr . Then expr [ VBAR Type To Then Step SEMI Recursive RPAR RBRACKET RBRACE Of Module LBRACE Function From End EOF Directive Const COMMA Block Begin Attr ARROW ]
##
## The known suffix of the stack is as follows:
## If expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: LPAR Verbatim While
##
## Ends in an error in state: 652.
##
## par(expr) -> LPAR expr . RPAR [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive DOT Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
## tuple(expr) -> expr . COMMA nsepseq(expr,COMMA) [ RPAR ]
##
## The known suffix of the stack is as follows:
## LPAR expr
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 658, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: Set LBRACKET Verbatim RBRACE
##
## Ends in an error in state: 687.
##
## injection(Set,expr) -> Set LBRACKET option(sep_or_term_list(expr,SEMI)) . RBRACKET [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LE LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ARROW ]
##
## The known suffix of the stack is as follows:
## Set LBRACKET option(sep_or_term_list(expr,SEMI))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 278, spurious reduction of production update_expr_level -> core_expr
## In state 157, spurious reduction of production unary_expr_level -> update_expr_level
## In state 161, spurious reduction of production mult_expr_level -> unary_expr_level
## In state 262, spurious reduction of production add_expr_level -> mult_expr_level
## In state 311, spurious reduction of production cons_expr_level -> add_expr_level
## In state 298, spurious reduction of production cat_expr_level -> cons_expr_level
## In state 338, spurious reduction of production comp_expr_level -> cat_expr_level
## In state 325, spurious reduction of production set_mem_level -> comp_expr_level
## In state 170, spurious reduction of production conj_expr_level -> set_mem_level
## In state 345, spurious reduction of production disj_expr_level -> conj_expr_level
## In state 296, spurious reduction of production base_expr(expr) -> disj_expr_level
## In state 351, spurious reduction of production expr -> base_expr(expr)
## In state 384, spurious reduction of production nsepseq(expr,SEMI) -> expr
## In state 383, spurious reduction of production sep_or_term_list(expr,SEMI) -> nsepseq(expr,SEMI)
## In state 663, spurious reduction of production option(sep_or_term_list(expr,SEMI)) -> sep_or_term_list(expr,SEMI)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

contract: Type Ident Is WILD TIMES Attr With
##
## Ends in an error in state: 41.
##
## record_type -> nseq(Attr) . ne_injection(Record,field_decl) [ VBAR Type TIMES SEMI Recursive RPAR RBRACKET RBRACE Module Is Function End EQ EOF Directive Const COMMA Attr ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## nseq(Attr)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 30, spurious reduction of production nseq(Attr) -> Attr
##

<YOUR SYNTAX ERROR MESSAGE HERE>

interactive_expr: LPAR Bytes RPAR DOT With
##
## Ends in an error in state: 174.
##
## local_path -> par(expr) DOT . nsepseq(selection,DOT) [ With VBAR Type To Then TIMES Step SLASH SHARP SEMI Recursive RPAR RBRACKET RBRACE PLUS Or Of NE Module Mod MINUS LT LPAR LE LBRACKET LBRACE GT GE Function From End Else EQ EOF Directive Contains Const COMMA COLON CARET Block Begin Attr And ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## par(expr) DOT
##

<YOUR SYNTAX ERROR MESSAGE HERE>
