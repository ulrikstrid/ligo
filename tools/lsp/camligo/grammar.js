module.exports = grammar({
  name: 'CameLigo',
  extras: $ => [$.comment, $.ocaml_comment, /\s/],

  rules: {


    contract: $ => repeat($.declaration),

    declaration: $ => choice(
      $.let_decl,
      $.type_decl
    ),

    type_annot: $ => seq(":", field("annotExpr", $.type_expr)),
    argument: $ => seq("(", field("argPattern", $.pattern), field("argAnnot", $.type_annot), ")"),
    let_decl: $ => seq(
      "let",
      optional(field("recursive", "rec")),
      field("bindName", $.Name),
      repeat(field("bindArgument", $.argument)),
      optional(field("bindAnnot", $.type_annot)),
      "=",
      field("letExpr",$.expr)),

    //========== EXPR ============

    let_expr: $ => seq(
      $.let_decl,
      "in",
      $.expr
    ),

    // [1;2]
    list_pattern: $ => seq(
      "[",
      optional(seq(
        field("patternListItem", $.pattern),
        repeat(seq(";", field("patternListItem", $.pattern)))
      )),
      "]"
    ),
    // a :: b
    list_con_pattern: $ => prec.right(9, seq(
      field("patX", $.pattern),
      "::",
      field("patXs", $.pattern)
    )),

    // a, b, c
    tup_pattern: $ => prec(8,seq(
      field("tuplePatternItem", $.pattern),
      repeat1(seq(",", field("tuplePatternItem", $.pattern)))
    )),

    pattern: $ => choice(
      $.Name,
      $.paren_pattern,
      prec(10, seq(field("conPattern", $.data_con), optional(field("conArgPattern",$.pattern)))),
      $.literal,
      $.list_pattern,
      $.list_con_pattern,
      $.tup_pattern,
      "_"
    ),

    paren_pattern: $ => seq(
      "(",
      field("innerPattern", $.pattern),
      ")"
    ),

    // Dog as x -> f x
    matching: $ => seq(field("pattern", $.pattern), "->", field("matchingExpr", $.expr)),
    // Dog as x -> f x | Cat as y -> y
    matchings: $ => prec.right(seq(
      field("matching", $.matching),

      repeat(seq("|", field("matching", $.matching)))
    )),

    // f a
    fun_app: $ => prec.left(20, seq(field("appF", $.expr), field("appArg",$.expr))),
    // Cat a
    con_app: $ => prec(19, seq($.data_con, $.expr)),
    // a + b
    op_app: $ => choice(
      prec.left(16, seq(field("arg1", $.expr), field("op", "mod"), field("arg2", $.expr))),
      prec.left(15, seq(field("arg1", $.expr), field("op", "*"), field("arg2", $.expr))),
      prec.left(15, seq(field("arg1", $.expr), field("op", "/"), field("arg2", $.expr))),
      prec.left(14, seq(field("arg1", $.expr), field("op", "+"), field("arg2", $.expr))),
      prec.left(14, seq(field("arg1", $.expr), field("op", "-"), field("arg2", $.expr))),

      prec.left(13, seq(field("arg1", $.expr), field("op", "::"), field("arg2", $.expr))),

      prec.left(12, seq(field("arg1", $.expr), field("op", "@"), field("arg2", $.expr))),
      prec.left(12, seq(field("arg1", $.expr), field("op", "^"), field("arg2", $.expr))),

      prec.left(11, seq(field("arg1", $.expr), field("op", "&&"), field("arg2", $.expr))),
      prec.left(11, seq(field("arg1", $.expr), field("op", "||"), field("arg2", $.expr))),
      prec.left(10, seq(field("arg1", $.expr), field("op", "="), field("arg2", $.expr))),
      prec.left(10, seq(field("arg1", $.expr), field("op", "<>"), field("arg2", $.expr))),
      prec.left(10, seq(field("arg1", $.expr), field("op", "=="), field("arg2", $.expr))),
      prec.left(10, seq(field("arg1", $.expr), field("op", "!="), field("arg2", $.expr))),
      prec.left(10, seq(field("arg1", $.expr), field("op", "<"), field("arg2", $.expr))),
      prec.left(10, seq(field("arg1", $.expr), field("op", "<="), field("arg2", $.expr))),
      prec.left(10, seq(field("arg1", $.expr), field("op", ">"), field("arg2", $.expr))),
      prec.left(10, seq(field("arg1", $.expr), field("op", ">="), field("arg2", $.expr)))
    ),
    // ! a
    unary_op_app: $ => choice(
      prec(18, seq(field("unaryOp", "-"), field("arg", $.expr))),
    ),
    // a.field
    rec_accessor: $ => prec(21, seq(field("rec", $.expr), ".", field("label", $.label))),

    // { p with a = b; c = d }
    rec_expr: $ => seq(
      "{",
      optional(seq(field("updateTarget", $.Name), "with")),
      repeat1(field("assignment", $.rec_assignment)),
      "}"
    ),
    // a = b;
    rec_assignment: $ => seq(
      field("assignmentLabel", $.label),
      "=",
      field("assignmentExpr", $.expr),
      ";"
    ),

    // if a then b else c
    if_expr: $ => seq(
      "if",
      field("condition", $.expr),
      "then",
      field("thenBranch", $.expr),
      "else",
      field("elseBranch", $.expr)
    ),

    // match x with ...
    match_expr: $ => prec(1,seq(
      "match",
      field("matchTarget", $.expr),
      "with",
      field("matchings", $.matchings)
    )),

    lambda_expr: $ => seq(
      "fun",
      repeat1(field("lambdaArgument", $.argument)),
      "->",
      field("lambdaBody", $.expr)
    ),

    list_expr: $ => seq(
      "[",
      optional(seq(
        field("listItem", $.expr),
        repeat(seq(
          ";",
          field("listItem",$.expr)
        ))
      )),
      "]"
    ),

    tup_expr: $ => prec(9,seq(
      field("tupleItem", $.expr),
      repeat1(seq(
        ",",
        field("tupleItem", $.expr)))
    )),

    expr: $ => choice(
      $.Name,
      $.Name_Capital,
      $.literal,
      $.paren_expr,
      $.let_expr,
      $.fun_app,
      $.op_app,
      //$.unary_op_app,
      $.rec_accessor,
      $.rec_expr,
      $.if_expr,
      $.lambda_expr,
      $.match_expr,
      $.list_expr,
      $.tup_expr
    ),

    paren_expr: $ => seq(
      "(",
      field("innerExpr", $.expr),
      optional(field("exprAnnot", $.type_annot)),
      ")"
    ),


    //========== TYPE_EXPR ============
    // t, test, string, integer
    type_con: $ => $.Name,
    // Red, Green, Blue, Cat
    data_con: $ => $.Name_Capital,
    // a t, (a, b) t
    type_app: $ => prec(10,seq(
      choice(
        field("argument", $.type_expr),
        seq("(", field("argument", $.type_expr), repeat1(seq(",", field("argument", $.type_expr))), ")")
      ),
      field("typeAppCon", $.type_con)
    )),
    // string * integer
    type_product: $ => prec(5, seq(field("element", $.type_expr), repeat1(seq("*", field("element", $.type_expr))))),

    // int -> string
    type_fun: $ => prec.right(8,seq(field("domain", $.type_expr), "->", field("codomain", $.type_expr))),
    
    type_expr: $ => choice(
      $.type_fun,
      $.type_product,
      $.type_app,
      $.type_con
    ),

    paren_type_expr: $ => seq(
      "(",
      field("innerTypeExpr", $.type_expr),
      ")"
    ),

    // Cat of string, Person of string * string
    variant: $ => seq(field("constructor", $.data_con), optional(seq("of", field("constructor_data", $.type_expr)))),
    // Cat of string | Personn of string * string
    type_sum: $ => seq(field("variant", $.variant), optional(repeat(seq("|", field("variant", $.variant))))),

    // field : string * int
    label: $ => $.Name,
    type_rec_field: $ => seq(field("recLabel", $.label), ":", field("labelType", $.type_expr)),
    // { field1 : a; field2 : b }
    type_rec: $ => seq(
      "{",
      repeat1(seq(field("recField", $.type_rec_field), ";")),
      "}"
    ),

    type_def_body: $ => choice(
      $.type_sum,
      $.type_expr,
      $.type_rec
    ),
    type_def: $ => seq(field("typeName", $.type_con), optional(seq("=", field("typeValue", $.type_def_body)))),
    type_decl: $ => seq("type", field("typeDef", $.type_def)),

    literal: $ => choice(
      $.String,
      $.Int,
      $.Nat,
      $.Tez,
      $.Bytes,
      $.True,
      $.False,
      $.Unit
    ),
    String:       $ => /\"(\\.|[^"])*\"/,
    Int:          $ => /-?([1-9][0-9_]*|0)/,
    Nat:          $ => /([1-9][0-9_]*|0)n/,
    Tez:          $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes:        $ => /0x[0-9a-fA-F]+/,
    Name:         $ => /[a-z][a-zA-Z0-9_]*/,
    Name_Capital: $ => /[A-Z][a-zA-Z0-9_]*/,
    Keyword:      $ => /[A-Za-z][a-z]*/,

    False:         $ => 'false',
    True:          $ => 'true',
    Unit:          $ => '()',

    comment: $ => /\/\/[^\n]*\n/,
    ocaml_comment: $ =>
      seq(
        '(*',
        repeat(choice(
          $.ocaml_comment,
          /'([^'\\]|\\[\\"'ntbr ]|\\[0-9][0-9][0-9]|\\x[0-9A-Fa-f][0-9A-Fa-f]|\\o[0-3][0-7][0-7])'/,
          /"([^\\"]|\\(.|\n))*"/,
          /[A-Za-z_][a-zA-Z0-9_']*/,
          /[^('"{*A-Za-z_]+/,
          '(', "'", '*',
        )),
        '*)'
      ),
  }
});
