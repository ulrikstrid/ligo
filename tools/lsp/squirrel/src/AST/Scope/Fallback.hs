{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module AST.Scope.Fallback where

import Control.Arrow ((&&&))
import Control.Lens ((%~), (&))
import Control.Monad.Catch.Pure hiding (throwM)
import Control.Monad.State
import Control.Monad.Writer (Writer, WriterT, execWriterT, runWriter, tell)

import Data.Foldable (for_, toList)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)

import Duplo.Lattice
import Duplo.Pretty
import Duplo.Tree hiding (loop)

import AST.Pretty (PPableLIGO, TotalLPP, docToText, lppDialect)
import AST.Scope.Common
import AST.Scope.ScopedDecl
  (DeclarationSpecifics (..), Parameter (..), ScopedDecl (..), ValueDeclSpecifics (..),
  fillTypeIntoCon)
import AST.Scope.ScopedDecl.Parser (parseTypeDeclSpecifics)
import AST.Skeleton hiding (Type)
import Cli.Types
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Parser
import Product
import Range
import Util (foldMapM, unconsFromEnd)

data Fallback

instance HasLigoClient m => HasScopeForest Fallback m where
  scopeForest _ (SomeLIGO dialect ligo) msg =
    let sf = flip runReader dialect
           . runExceptT
           . getEnv
           $ ligo
        emptyScopeForest = ScopeForest [] Map.empty
        fallbackErrorMsg e@(TreeDoesNotContainName _ r _) = (r, Error (ppToText e) [])
    in case sf of
      Left e -> pure (emptyScopeForest, msg ++ [fallbackErrorMsg e])
      Right sf' -> pure (sf', msg)

addReferences :: LIGO Info -> ScopeForest -> ScopeForest
addReferences ligo = execState $ loopM_ addRef ligo
  where
    addRef :: LIGO Info -> State ScopeForest ()
    addRef = \case
      (match -> Just (r, Name     n)) -> addThisRef TermLevel (getRange r) n
      (match -> Just (r, NameDecl n)) -> addThisRef TermLevel (getRange r) n
      (match -> Just (r, Ctor     n)) -> addThisRef TermLevel (getRange r) n
      (match -> Just (r, TypeName n)) -> addThisRef TypeLevel (getRange r) n
      _                               -> return ()

    addThisRef cat' r n = do
      modify
        $ withScopeForest \(sf, ds) ->
          flip runState ds do
            let frameSet = Set.toList =<< spine r =<< sf
            walkScope cat' r n frameSet
            return sf

    walkScope _    _ _ [] = return ()
    walkScope level r n (declref : rest) = do
      decl <- gets (Map.! declref)
      if ofLevel level decl && (n == _sdName decl || r == _sdOrigin decl)
      then do
        modify $ Map.adjust (addRefToDecl r) declref
      else do
        walkScope level r n rest

    addRefToDecl r sd = sd { _sdRefs = r : _sdRefs sd }

getEnv :: LIGO Info -> ScopeM ScopeForest
getEnv tree
  = addReferences tree
  . extractScopeForest
  . compressScopeTree
  . extractScopeTree
  <$> prepareTree tree

prepareTree
  :: LIGO Info
  -> ScopeM (LIGO '[[ScopedDecl], Bool, Range, [Text], Range, ShowRange, CodeSource])
prepareTree
  = assignDecls
  . wildcardToName
  . unSeq
  . unLetRec

loop :: Functor f => (Cofree f a -> Cofree f a) -> Cofree f a -> Cofree f a
loop go = aux
  where
    aux (r :< fs) = go $ r :< fmap aux fs

loopM_ :: (Monad m, Foldable f) => (Cofree f a -> m ()) -> (Cofree f a -> m ())
loopM_ go = aux
  where
    aux (r :< fs) = do
      for_ fs aux
      go $ r :< fs

loopM
  :: (Monad m, Traversable f)
  => (Cofree f a -> m (Cofree f a)) -> (Cofree f a -> m (Cofree f a))
loopM go = aux
  where
    aux (r :< fs) = go =<< ((r :<) <$> traverse aux fs)

-- | Replace a wildcard with a dummy "_" name so that the LSP doesn't trip up
-- with 'TreeDoesNotContainName'. This is also necessary so that we are able to
-- use capabilities inside wildcards declarations, such as the definition of `x`
-- in `let _ = x * x`.
wildcardToName
  :: ( Contains Range xs
     , Eq (Product xs)
     )
  => LIGO xs
  -> LIGO xs
wildcardToName = loop go
  where
    go (match -> Just (r, IsWildcard)) = make (r, NameDecl "_")
    go it = it

unLetRec
  :: ( Contains  Range     xs
     , Eq (Product xs)
     )
  => LIGO xs
  -> LIGO xs
unLetRec = loop go
  where
    go = \case
      (match -> Just (r, expr)) -> do
        case expr of
          Let (match -> Just (_, Seq decls)) body -> do
            foldr joinWithLet body decls
          _ -> make (r, expr)

      -- TODO: somehow append Unit to the end
      (match -> Just (r, RawContract decls)) -> do
        case unconsFromEnd decls of
          Nothing -> make (r, RawContract [])
          Just (initDecls, lastDecl) -> foldr joinWithLet lastDecl initDecls

      it -> it

-- | Turn all 'Seq'uences of nodes into a tree of 'Let's, so that each
-- subsequent node turned out to be in the scope of the previous one.
unSeq
  :: ( Contains  Range     xs
     , Eq (Product xs)
     )
  => LIGO xs
  -> LIGO xs
unSeq = loop go
  where
    go = \case
      (match -> Just (r, Seq decls)) -> do
        case unconsFromEnd decls of
          Nothing -> make (r, Seq [])
          Just (initDecls, lastDecl) -> foldr joinWithLet lastDecl initDecls

      it -> it

-- | Combine two tree nodes with 'Let', so that the second node turns out to be
-- in the scope of the first node.
joinWithLet
  :: ( Contains Range xs
     , Apply Functor fs
     , Eq (Product xs)
     , Element Expr fs
     )
  => Tree' fs xs -> Tree' fs xs -> Tree' fs xs
joinWithLet decl body = make (r', Let decl body)
  where
    r' = putElem (getRange decl `merged` getRange body)
        $ extract body

assignDecls
  :: ( Contains  Range     xs
     , Contains [Text]     xs
     , Contains  ShowRange xs
     , Eq (Product xs)
     )
  => LIGO xs
  -> ScopeM (LIGO ([ScopedDecl] : Bool : Range : xs))
assignDecls = loopM go . fmap (\r -> [] :> False :> getRange r :> r)
  where
    go = \case
      (match -> Just (r, Let decl body)) -> do
        imm <- getImmediateDecls decl
        let r' :< body' = body
        let l' :< decl' = decl
        let r'' = putElem (getRange body) $ putElem True $ modElem (imm <>) r'
        let l'' = putElem (getRange decl) $ putElem True l'
        pure (make (r, Let (l'' :< decl') (r'' :< body')))

      (match -> Just (r, Lambda args ty body)) -> do
        imms <- foldMapM getImmediateDecls args
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        pure (make (r, Lambda args ty (r'' :< body')))

      (match -> Just (r, Alt pat body)) -> do
        imms <- getImmediateDecls pat
        let r' :< body' = body
        let r'' = putElem True $ modElem (imms <>) r'
        pure (make (r, Alt pat (r'' :< body')))

      (match -> Just (r, BFunction True n params ty b)) -> do
        imms <- foldMapM getImmediateDecls params
        fDecl <- functionScopedDecl (getElem r) n params ty (Just b)
        let r' = putElem True $ modElem ((fDecl : imms) <>) r
        pure (make (r', BFunction True n params ty b))

      (match -> Just (r, BFunction False n params ty b)) -> do
        imms <- foldMapM getImmediateDecls params
        fDecl <- functionScopedDecl (getElem r) n params ty (Just b)
        let r' = putElem True (modElem (fDecl :) r)
        let b' = b & _extract %~ (putElem True . modElem (imms <>))
        pure (make (r', BFunction False n params ty b'))

      (match -> Just (r, node@BVar{})) -> markAsScope r node
      (match -> Just (r, node@BConst{})) -> markAsScope r node
      (match -> Just (r, node@BParameter{})) -> markAsScope r node
      (match -> Just (r, node@IsVar{})) -> markAsScope r node
      (match -> Just (r, node@BTypeDecl{})) -> markAsScope r node

      it -> pure it

    markAsScope range node = do
      imms <- getImmediateDecls (make (range, node))
      let range' = putElem True (modElem (imms <>) range)
      pure (make (range', node))

functionScopedDecl
  :: ( TotalLPP param
     , Eq (Product info)
     , PPableLIGO info
     )
  => [Text] -- ^ documentation comments
  -> LIGO info -- ^ name node
  -> [param] -- ^ parameter nodes
  -> Maybe (LIGO info) -- ^ type node
  -> Maybe (LIGO info) -- ^ function body node, optional for type constructors
  -> ScopeM ScopedDecl
functionScopedDecl docs nameNode paramNodes typ body = do
  dialect <- lift ask
  (origin, name) <- getName nameNode
  let _vdsInitRange = getRange <$> body
      _vdsParams = pure (params dialect)
      _vdsTspec = parseTypeDeclSpecifics <$> typ
  pure $ ScopedDecl
    { _sdName = name
    , _sdOrigin = origin
    , _sdRefs = []
    , _sdDoc = docs
    , _sdDialect = dialect
    , _sdSpec = ValueSpec ValueDeclSpecifics{ .. }
    }
  where
    params dialect = map (Parameter . docToText . lppDialect dialect) paramNodes

valueScopedDecl
  :: ( Eq (Product info)
     , PPableLIGO info
     )
  => [Text] -- ^ documentation comments
  -> LIGO info -- ^ name node
  -> Maybe (LIGO info) -- ^ type node
  -> Maybe (LIGO info) -- ^ initializer node
  -> ScopeM ScopedDecl
valueScopedDecl docs nameNode typ body = do
  dialect <- lift ask
  (origin, name) <- getName nameNode
  pure $ ScopedDecl
    { _sdName = name
    , _sdOrigin = origin
    , _sdRefs = []
    , _sdDoc = docs
    , _sdDialect = dialect
    , _sdSpec = ValueSpec ValueDeclSpecifics{ .. }
    }
  where
    _vdsInitRange = getRange <$> body
    _vdsParams = Nothing
    _vdsTspec = parseTypeDeclSpecifics <$> typ

typeScopedDecl
  :: ( Eq (Product info)
     , PPableLIGO info
     )
  => [Text] -> LIGO info -> LIGO info -> ScopeM ScopedDecl
typeScopedDecl docs nameNode body = do
  dialect <- lift ask
  (origin, name) <- getTypeName nameNode
  pure $ ScopedDecl
    { _sdName = name
    , _sdOrigin = origin
    , _sdRefs = []
    , _sdDoc = docs
    , _sdDialect = dialect
    , _sdSpec = TypeSpec (parseTypeDeclSpecifics body)
    }

-- | Wraps a value into a list. Like 'pure' but perhaps with a more clear intent.
singleton :: a -> [a]
singleton x = [x]

extractScopeTree
  :: LIGO ([ScopedDecl] : Bool : Range : xs)
  -> Tree' '[[]] '[[ScopedDecl], Bool, Range]
extractScopeTree ((decls :> visible :> r :> _) :< fs)
  = make (decls :> visible :> r :> Nil, map extractScopeTree (toList fs))

-- 'Bool' in the node list denotes whether this part of a tree is a scope
compressScopeTree
  :: Tree' '[[]] '[[ScopedDecl], Bool, Range]
  -> [Tree' '[[]] '[[ScopedDecl], Range]]
compressScopeTree = go
  where
    go
      :: Tree' '[[]] '[[ScopedDecl], Bool, Range]
      -> [Tree' '[[]] '[[ScopedDecl], Range]]
    go (only -> (_ :> False :> _ :> Nil, rest)) =
      rest >>= go

    go (only -> (decls :> True :> r :> Nil, rest)) =
      let rest' = rest >>= go
      in [ make (decls :> r :> Nil, rest')
         | not (null decls) || not (null rest')
         ]

    go _ = error "compressScopeTree: impossible"

extractScopeForest
  :: [Tree' '[[]] '[[ScopedDecl], Range]]
  -> ScopeForest
extractScopeForest = uncurry ScopeForest . runWriter . mapM go
  where
    go
      :: Tree' '[[]] '[[ScopedDecl], Range]
      -> Writer (Map DeclRef ScopedDecl) ScopeTree
    go (only -> (decls :> r :> Nil, ts)) = do
      let mkDeclRef sd = DeclRef (ppToText (_sdName sd)) (_sdOrigin sd)
      let extracted = Map.fromList $ map (mkDeclRef &&& id) decls
      tell extracted
      let refs    = Map.keysSet extracted
      let r'      = refs :> r :> Nil
      ts' <- mapM go ts
      return $ make (r', ts')

    go _ = error "extractScopeForest: impossible"

getImmediateDecls
  :: ( PPableLIGO info
     , Eq (Product info)
     )
  => LIGO info -> ScopeM [ScopedDecl]
getImmediateDecls = \case
  (match -> Just (r, pat)) -> do
    case pat of
      IsVar v ->
        singleton <$> valueScopedDecl (getElem r) v Nothing Nothing

      IsTuple    xs   -> foldMapM getImmediateDecls xs
      IsRecord   xs   -> foldMapM getImmediateDecls xs
      IsList     xs   -> foldMapM getImmediateDecls xs
      IsSpread   s    -> getImmediateDecls s
      IsWildcard      -> pure []
      IsAnnot    x t  -> (<>) <$> getImmediateDecls x <*> getImmediateDecls t
      IsCons     h t  -> (<>) <$> getImmediateDecls h <*> getImmediateDecls t
      IsConstant _    -> pure []
      IsConstr   _ xs -> foldMapM getImmediateDecls xs

  (match -> Just (r, pat)) -> do
    case pat of
      IsRecordField label body ->
        singleton <$> valueScopedDecl (getElem r) label Nothing (Just body)

  (match -> Just (r, pat)) -> do
    case pat of
      BFunction _ f params t b ->
        singleton <$> functionScopedDecl (getElem r) f params t (Just b)

      BVar v t b -> singleton <$> valueScopedDecl (getElem r) v t b

      BConst name typ (Just (layer -> Just (Lambda params _ body))) ->
        singleton <$> functionScopedDecl (getElem r) name params typ (Just body)

      BConst c t b -> singleton <$> valueScopedDecl (getElem r) c t b

      BParameter n t ->
        singleton <$> valueScopedDecl (getElem r) n (Just t) Nothing

      BTypeDecl t b -> do
        typeDecl <- typeScopedDecl (getElem r) t b
        -- Gather all other declarations from the depths of ast, such as type
        -- sum constructors, nested types etc. Then, fill in missing types of
        -- values. It doesn't seem possible that these values will be anything
        -- but directly related to the type constructors. There are two reasons
        -- for that. One is that deeper nested constructors will have their
        -- types already filled by types corresponding to them. Two is that due
        -- to grammar limitations there will be no other values besides
        -- constructors.
        structureDecls <- getImmediateDecls b
                      <&> map (fillTypeIntoCon typeDecl)
        pure (typeDecl : structureDecls)

      BAttribute _ -> pure []
      BInclude _ -> pure []

  (match -> Just (_, pat)) -> case pat of
    TRecord typeFields -> foldMapM getImmediateDecls typeFields
    TProduct typs -> foldMapM getImmediateDecls typs
    TSum variants -> foldMapM getImmediateDecls variants
    _ -> pure []
    -- there are most probably others, add them as problems arise

  (match -> Just (r, Variant name paramTyp)) -> do
    -- type is Nothing at this stage, but it will be substituted with the
    -- (hopefully) correct type higher in the tree (see 'BTypeDecl' branch).
    constructorDecl <- functionScopedDecl (getElem r) name [paramTyp] Nothing Nothing
    nestedDecls <- maybe (pure []) getImmediateDecls paramTyp
    pure (constructorDecl : nestedDecls)
  _ -> pure []

select
  :: ( PPableLIGO info
     , MonadError ScopeError m
     )
  => Text
  -> [Visit RawLigoList (Product info) (WriterT [LIGO info] Catch)]
  -> LIGO info
  -> m (Range, Text)
select what handlers t
  = maybe
      (throwError $ TreeDoesNotContainName (pp t) (getRange t) what)
      (return . (getElem . extract &&& ppToText))
  $ either (const Nothing) listToMaybe
  $ runCatch
  $ execWriterT
  $ visit handlers
    t

getName
  :: ( Lattice  (Product info)
     , PPableLIGO info
     , MonadError ScopeError m
     )
  => LIGO info
  -> m (Range, Text)
getName = select "name"
  [ Visit \(r, NameDecl t) -> do
      tell [make (r, Name t)]
  , Visit \(r, Ctor t) -> do
      tell [make (r, Name t)]
  ]

getTypeName
  :: ( Lattice  (Product info)
     , PPableLIGO info
     , MonadError ScopeError m
     )
  => LIGO info
  -> m (Range, Text)
getTypeName = select "type name"
  [ Visit \(r, TypeName t) -> do
      tell [make (r, TypeName t)]
  ]
