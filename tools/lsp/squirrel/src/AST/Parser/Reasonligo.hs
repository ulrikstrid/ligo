-- | Parser for ReasonLigo contract

module AST.Parser.Reasonligo where

import AST.Skeleton

import Duplo.Tree

import ParseTree
import Parser
  (CodeSource (CodeSource), Info, ParserM, ShowRange (N), boilerplate, boilerplate', fallthrough,
  field, fieldOpt, fields, withComments)
import Product


recognise :: SomeRawTree -> ParserM (SomeLIGO Info)
recognise (SomeRawTree dialect rawTree)
  = fmap (SomeLIGO dialect)
  $ flip (descent (error "Reasonligo.recognise")) rawTree
  $ map usingScope
  [ -- Contract
    Descent do
      boilerplate $ \case
        "source_file" -> RawContract <$> fields "declaration"
        _ -> fallthrough

    -- Expr
  , Descent do
      boilerplate $ \case
        "unary_call"  -> UnOp       <$> field  "negate"    <*> field "arg"
        "binary_call" -> BinOp      <$> field  "left"      <*> field "op"        <*> field "right"
        "Some_call"   -> Apply      <$> field  "some"      <*> fields "argument"
        "apply"       -> Apply      <$> field  "function"  <*> fields "argument"
        "block"       -> Seq        <$> fields "statement"
        "list"        -> List       <$> fields "element"
        "indexing"    -> ListAccess <$> field  "box"       <*> fields "index"
        "annot_expr"  -> Annot      <$> field  "subject"   <*> field "type"
        "if"          -> If         <$> field  "selector"  <*> field "then"      <*> fieldOpt "else"
        "record"      -> Record     <$> fields "assignment"
        "record_update" -> RecordUpd <$> field  "subject" <*> fields "field"
        "tuple"       -> Tuple      <$> fields "item"
        "switch"      -> Case       <$> field  "subject"   <*> fields   "alt"
        "lambda"      -> Lambda     <$> fields "argument"  <*> fieldOpt "type"   <*> field "body"
        "michelson_interop" -> Michelson  <$> field  "code"      <*> field "type"  <*> fields "argument"
        "let_in"      -> Let        <$> field "declaration"      <*> field "body"
        "paren_expr"  -> Paren      <$> field "expr"
        _             -> fallthrough

    -- Pattern
  , Descent do
      boilerplate $ \case
        "tuple_pattern"          -> IsTuple <$> fields "pattern"
        "annot_pattern"          -> IsAnnot <$> field "subject" <*> field "type"
        "list_pattern"           -> IsList  <$> fields "pattern"
        "var_pattern"            -> IsVar   <$> field "var"
        "wildcard"               -> return IsWildcard
        "nullary_constr_pattern" -> IsConstr <$> field "constructor" <*> return Nothing
        "unary_constr_pattern"   -> IsConstr <$> field "constructor" <*> fieldOpt "arg"
        "spread_pattern"         -> IsSpread <$> field "expr"
        "record_pattern"         -> IsRecord <$> fields "field"
        _                        -> fallthrough

    -- Irrefutable tuple
  , Descent do
      boilerplate $ \case
        "irrefutable_tuple" -> IsTuple <$> fields "item"
        _                   -> fallthrough

    -- RecordFieldPattern
  , Descent do
      boilerplate $ \case
        "record_field_pattern"  -> IsRecordField <$> field "name" <*> field "body"
        _                       -> fallthrough

    -- Alt
  , Descent do
      boilerplate $ \case
        "alt"  -> Alt <$> field "pattern" <*> field "expr"
        _                   -> fallthrough

    -- Record fields
    -- TODO: capture and record
  , Descent do
      boilerplate $ \case
        "record_field" -> FieldAssignment <$> fields "accessor" <*> field "value"
        "record_field_path" -> FieldAssignment <$> fields "accessor" <*> field "value"
        "spread" -> Spread <$> field "name"
        _ -> fallthrough

    -- Preprocessor
  , Descent do
      boilerplate \case
        "preprocessor" -> Preprocessor <$> field "preprocessor_command"
        _              -> fallthrough

    -- ProcessorCommand
  , Descent do
      boilerplate' \case
        ("p_if"      , rest) -> return $ PreprocessorCommand $ "#if "      <> rest
        ("p_error"   , rest) -> return $ PreprocessorCommand $ "#error "   <> rest
        ("p_warning" , rest) -> return $ PreprocessorCommand $ "#warning " <> rest
        ("p_define"  , rest) -> return $ PreprocessorCommand $ "#define "  <> rest
        _                    -> fallthrough

    -- MapBinding
  , Descent do
      boilerplate $ \case
        "binding" -> MapBinding <$> field "key" <*> field "value"
        _         -> fallthrough

  , Descent do
      boilerplate' $ \case
        ("+", _) -> return $ Op "+"
        ("-", _) -> return $ Op "-"
        ("mod", _) -> return $ Op "mod"
        ("/", _) -> return $ Op "/"
        ("*", _) -> return $ Op "*"
        (">", _) -> return $ Op ">"
        ("<", _) -> return $ Op "<"
        (">=", _) -> return $ Op ">="
        ("<=", _) -> return $ Op "<="
        ("==", _) -> return $ Op "=="
        ("!=", _) -> return $ Op "!="
        ("||", _) -> return $ Op "||"
        ("&&", _) -> return $ Op "&&"
        ("negate", n) -> return $ Op n
        _         -> fallthrough

  , Descent do
      boilerplate $ \case
        "data_projection" -> QualifiedName <$> field "expr" <*> fields "accessor"
        _ -> fallthrough

    -- Literal
  , Descent do
      boilerplate' $ \case
        ("Int",    i) -> return $ Int i
        ("Nat",    i) -> return $ Nat i
        ("Bytes",  i) -> return $ Bytes i
        ("String", i) -> return $ String i
        ("Tez",    i) -> return $ Tez i
        _             -> fallthrough

    -- Declaration
  , Descent do
      boilerplate $ \case
        -- TODO: We forget "rec" field in let
        "let_declaration" -> BConst <$> field "binding" <*> fieldOpt "type" <*> fieldOpt "value"
        "type_decl" -> BTypeDecl <$> field "type_name" <*> field "type_value"
        "attr_decl" -> BAttribute <$> field "name"
        "include"   -> BInclude  <$>                      field "filename"
        _ -> fallthrough

    -- MichelsonCode
  , Descent do
      boilerplate' \case
        ("michelson_code", code) -> return $ MichelsonCode code
        _                        -> fallthrough

    -- Name
  , Descent do
      boilerplate' $ \case
        ("Name", n) -> return $ Name n
        ("and", _)  -> return $ Name "and"
        ("or", _)   -> return $ Name "or"
        _           -> fallthrough

    -- NameDecl
  , Descent do
      boilerplate' $ \case
        ("NameDecl", n) -> return $ NameDecl n
        _               -> fallthrough

    -- NameModule
  , Descent do
      boilerplate' $ \case
        ("NameModule", n) -> return $ NameModule n
        _                 -> fallthrough

    -- Type
  , Descent do
      boilerplate $ \case
        "fun_type"         -> TArrow   <$> field  "domain"     <*> field "codomain"
        "type_application" -> TApply   <$> field  "functor" <*> fields "argument"
        "type_tuple"       -> TProduct <$> fields "element"
        "record_type"      -> TRecord  <$> fields "field"
        "sum_type"         -> TSum     <$> fields "variant"
        "TypeWildcard"     -> pure TWildcard
        _                  -> fallthrough

    -- Module access:
  , Descent do
      boilerplate $ \case
        "module_TypeName" -> ModuleAccess <$> fields "path" <*> field "type"
        "module_access"   -> ModuleAccess <$> fields "path" <*> field "field"
        _                 -> fallthrough

   -- Michelson pair types
  , Descent do
      boilerplate' $ \case
        ("type_string", i) -> return $ TString i
        _                  -> fallthrough


    -- Variant
  , Descent do
      boilerplate $ \case
        "variant" -> Variant <$> field "constructor" <*> fieldOpt "arguments"
        _         -> fallthrough

    -- TField
  , Descent do
      boilerplate $ \case
        "field_decl" -> TField <$> field "field_name" <*> field "field_type"
        _            -> fallthrough

    -- TypeName
  , Descent do
      boilerplate' $ \case
        ("TypeName", name) -> return $ TypeName name
        _                  -> fallthrough

    -- FieldName
  , Descent do
      boilerplate' $ \case
        ("FieldName", name) -> return $ FieldName name
        _                   -> fallthrough

    -- Ctor
  , Descent do
      boilerplate' $ \case
        ("ConstrName", name)   -> return $ Ctor name
        ("Some", _)            -> return $ Ctor "Some"
        ("None", _)            -> return $ Ctor "None"
        ("Bool", b)            -> return $ Ctor b
        ("Unit", _)            -> return $ Ctor "Unit"
        ("Nil", _)             -> return $ Ctor "Nil"
        _                      -> fallthrough

  -- Err
  , Descent do
      \(r :> _, ParseTree _ children source) -> do
        withComments do
          return (r :> N :> CodeSource source :> Nil, Error source children)
  ]
