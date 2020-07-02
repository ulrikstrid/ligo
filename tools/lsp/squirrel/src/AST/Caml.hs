module AST.Caml where

import Data.Text (Text)
import Data.Sum

import AST.Types

import Parser
import Tree hiding (skip)


ranged
  :: ( Functor f
     , Element f fs
     )
  => Parser (f (Tree fs ASTInfo))
  -> Parser    (Tree fs ASTInfo)
ranged p = do
  r <- getInfo
  a <- p
  return $ mk r a

name :: Parser (Caml ASTInfo)
name = ranged do pure Name <*> token "Name"

capitalName :: Parser (Caml ASTInfo)
capitalName = ranged do pure Name <*> token "Name_Capital"

literal :: Parser (Caml ASTInfo)
literal = subtree "literal" . ranged $ select
  [ String <$> token "String"
  , Int <$> token "Int"
  , Nat <$> token "Nat"
  , Tez <$> token "Tez"
  , Bytes <$> token "Bytes"
  , Unit <$ token "Unit"
  , Name <$> token "False"
  , Name <$> token "True"
  ]

contract :: Parser (Caml ASTInfo)
contract = ranged $
  Contract
  <$> subtree "contract" do
        many do
          declaration

declaration :: Parser (Caml ASTInfo)
declaration = subtree "declaration" $
      (ranged $ ValueDecl <$> letDecl)
  <|> typeDecl
  -- <|> include

typeDecl :: Parser (Caml ASTInfo)
typeDecl = do
  subtree "type_decl" $
    inside "typeDef" typeDef

typeDef :: Parser (Caml ASTInfo)
typeDef = subtree "type_def" . ranged $
  TypeDecl
  <$> inside "typeName" typeCon
  <*> inside "typeValue" typeDefBody

typeDefBody :: Parser (Caml ASTInfo)
typeDefBody = subtree "type_def_body" $ select
  [ typeSum
  , typeExpr
  , typeRec
  ]

typeSum :: Parser (Caml ASTInfo)
typeSum = do
  subtree "type_sum" . ranged $ do
    pure TSum <*> many do
      inside "variant" variant
  
variant :: Parser (Caml ASTInfo)
variant = do
  subtree "variant" . ranged $ do
    pure Variant
      <*> inside "constructor" dataCon
      <*> optional do inside "constructor_data" typeExpr

dataCon :: Parser (Caml ASTInfo)
dataCon = do
  subtree "data_con" capitalName

typeCon :: Parser (Caml ASTInfo)
typeCon = do
  subtree "type_con" name

label :: Parser (Caml ASTInfo)
label = do
  subtree "label" name

typeExpr :: Parser (Caml ASTInfo)
typeExpr = do
  subtree "type_expr" $ select
    [ typeFun
    , typeProduct
    , typeApp
    , typeCon
    , parenTypeExpr
    ]
  where
    parenTypeExpr = subtree "paren_type_expr" . inside "innerTypeExpr" $ typeExpr

typeFun :: Parser (Caml ASTInfo)
typeFun = do
  subtree "type_fun" . ranged $ do
    TArrow <$> inside "domain" typeExpr <*> inside "codomain" typeExpr

typeProduct :: Parser (Caml ASTInfo)
typeProduct = do
  subtree "type_product" . ranged $ do
    pure TProduct <*> many do
      inside "element" typeExpr

typeApp :: Parser (Caml ASTInfo)
typeApp =
  subtree "type_app" . ranged $ do
    args <- many (inside "argument" typeExpr)
    TApply <$> inside "typeAppCon" typeCon <*> pure args

typeRec :: Parser (Caml ASTInfo)
typeRec =
  subtree "type_rec" . ranged $ do
    TRecord <$>
      (many . inside "recField" . subtree "type_rec_field" . ranged $
        TField <$> inside "recLabel" label <*> inside "labelType" typeExpr)

typeAnnotation :: Parser (Caml ASTInfo)
typeAnnotation = do
  subtree "type_annot" . inside "annotExpr" $ typeExpr

letDecl :: Parser (Caml ASTInfo)
letDecl =
  subtree "let_decl" . ranged $ do
    Function
    <$> recursive
    <*> inside "bindName" name
    <*> many (inside "bindArgument" argument)
    <*> inside "bindAnnot" typeAnnotation
    <*> inside "letExpr" expr
  where
    recursive =
      maybe False (const True) <$> optional (inside "recursive" (token "rec"))

argument :: Parser (Caml ASTInfo)
argument =
  subtree "argument" . ranged $ do
    Decl
    <$> ranged (pure Immutable)
    <*> inside "argPattern" pattern
    <*> inside "argAnnot" typeAnnotation

pattern :: Parser (Caml ASTInfo)
pattern = do
  subtree "pattern" $ select (
    [ ranged $ IsVar <$> name
    , ranged $ IsConstr <$> inside "conPattern" dataCon <*> optional (inside "conArgPattern" pattern)
    , ranged $ IsConstant <$> literal
    , listPattern
    , listConPattern
    , tuplePattern
    , ranged $ IsWildcard <$ token "_"
    , parenPattern
    ] :: [Parser (Caml ASTInfo)])
  where
    parenPattern = subtree "paren_pattern" . inside "innerPattern" $ pattern

listPattern :: Parser (Caml ASTInfo)
listPattern =
  subtree "list_pattern" . ranged $
    IsList <$> many (inside "listPatternItem" pattern)

listConPattern :: Parser (Caml ASTInfo)
listConPattern = do
  subtree "list_con_pattern" . ranged $
    IsCons
    <$> inside "patX" pattern
    <*> inside "patXs" pattern

tuplePattern :: Parser (Caml ASTInfo)
tuplePattern =
  subtree "tup_pattern" . ranged $
    IsTuple <$> many (inside "tuplePatternItem" pattern)

expr :: Parser (Caml ASTInfo)
expr =
  subtree "expr" $ select
    [ ranged $ Ident <$> name 
    , ranged $ Constant <$> literal
    , parenExpr
    , funApp
    , opApp
    , prefixOpApp
    , recAccessor
    , ifExpr
    , lambdaExpr
    , matchExpr
    , tupExpr
    , listExpr
    , recExpr
    ]
  where
    parenExpr = subtree "paren_expr" $ do
      e <- inside "innerExpr" expr
      mAnnot <- optional (inside "exprAnnot" typeAnnotation)
      case mAnnot of
        Nothing -> pure e
        Just annot -> ranged . pure $ Annot e annot

funApp :: Parser (Caml ASTInfo)
funApp =
  subtree "fun_app" . ranged $
    Apply <$> inside "appF" expr <*> ((:[]) <$> inside "appArg" expr)

opApp :: Parser (Caml ASTInfo)
opApp =
  subtree "op_app" . ranged $ do
    l <- inside "arg1" expr
    op <- inside "op" anything
    r <- inside "arg2" expr
    pure $ BinOp l op r

prefixOpApp :: Parser (Caml ASTInfo)
prefixOpApp =
  subtree "unary_op_app" . ranged $
    UnOp <$> inside "unaryOp" anything <*> inside "arg" expr

recAccessor :: Parser (Caml ASTInfo)
recAccessor =
  subtree "rec_accessor" . ranged $
    RecAccessor <$> inside "rec" expr <*> inside "label" label

ifExpr :: Parser (Caml ASTInfo)
ifExpr =
  subtree "if_expr" . ranged $
    If <$> inside "condition" expr <*> inside "thenBranch" expr <*> inside "elseBranch" expr

lambdaExpr :: Parser (Caml ASTInfo)
lambdaExpr = 
  subtree "lambda_expr" . ranged $
    Lambda
    <$> many (inside "lambdaArgument" argument)
    <*> pure Nothing
    <*> inside "lambdaBody" expr

matchExpr :: Parser (Caml ASTInfo)
matchExpr =
  subtree "match_expr" . ranged $ do
    Case
    <$> inside "matchTarget" expr
    <*> inside "matchings" matchings

matchings :: Parser [Caml ASTInfo]
matchings = do
  subtree "matchings" $
    many (inside "matching" matching)

matching :: Parser (Caml ASTInfo)
matching =
  subtree "matching" . ranged $ do
    Alt
    <$> inside "pattern" pattern
    <*> inside "matchingExpr" expr

tupExpr :: Parser (Caml ASTInfo)
tupExpr = subtree "tup_expr" . ranged $
  Tuple <$> many (inside "tupleItem" expr)

listExpr :: Parser (Caml ASTInfo)
listExpr = subtree "list_expr" . ranged $
  List <$> many (inside "listItem" expr)

recExpr :: Parser (Caml ASTInfo)
recExpr = subtree "rec_expr" . ranged $ do
  upd <- optional $ inside "updateTarget" name
  assignments <- many (inside "assignment" assignment)
  case upd of
    Nothing -> pure $ Record assignments
    Just u -> pure $ RecordUpd u assignments
  where
    assignment = subtree "rec_assignment" . ranged $
      Assignment <$> inside "assignmentLabel" label <*> inside "assignmentExpr" expr
