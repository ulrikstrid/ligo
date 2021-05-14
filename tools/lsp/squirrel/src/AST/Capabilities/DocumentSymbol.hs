{-# LANGUAGE RecordWildCards #-}

module AST.Capabilities.DocumentSymbol where

import Control.Lens ((^.))
import Control.Monad.Catch.Pure (MonadCatch)
import Control.Monad.Writer.Strict
import Data.Maybe (fromMaybe)
import Data.Text
import Duplo (match)
import Language.LSP.Types (SymbolInformation (..))
import qualified Language.LSP.Types as J

import AST.Capabilities.Find
import AST.Scope
import AST.Scope.ScopedDecl (ScopedDecl (..))
import AST.Skeleton
import Product
import Range


-- | Extract document symbols for some specific parsed ligo contract which
-- is realisable by @haskell-lsp@ client.
extractDocumentSymbols
  :: forall m.
     (MonadCatch m)
  => J.Uri
  -> SomeLIGO Info'
  -> m [SymbolInformation]
extractDocumentSymbols uri tree =
  execWriterT $ collectFromContract (tree ^. nestedLIGO)
  where
    collectFromContract :: LIGO Info' -> WriterT [SymbolInformation] m ()
    collectFromContract (match @RawContract -> Just (_, RawContract decls))
      = mapM_ collectDecl decls
    collectFromContract (match @Contract-> Just (_, ContractCons contr contrs))
      = collectFromContract contr *> collectFromContract contrs
    collectFromContract _
      = pure ()

    collectDecl :: LIGO Info' -> WriterT [SymbolInformation] m ()
    collectDecl (match @Binding -> Just (_, binding)) = case binding of
          (BFunction _ (match @NameDecl -> Just (getElem @Range -> r, _)) _ _ _)->
            tellScopedDecl
              r
              J.SkFunction
              (const Nothing)

          -- TODO: currently we do not count imports as declarations in scopes
          (BInclude (match @Constant -> Just (getElem @Range -> r, _))) ->
            tellSymbolInfo
              r
              J.SkNamespace
              ("some import at " <> pack (show r))

          (BTypeDecl (match @TypeName -> Just (getElem @Range -> r, _)) _) ->
            tellScopedDecl
              r
              J.SkTypeParameter
              (const Nothing)

          (BConst (match @NameDecl -> Just (getElem @Range -> r, _)) _ _) ->
            tellScopedDecl
              r
              J.SkConstant
              (\ScopedDecl {_sdName} -> Just ("const " <> _sdName))

          (BVar (match @NameDecl -> Just (getElem @Range -> r, _)) _ _) ->
            tellScopedDecl
              r
              J.SkVariable
              (\ScopedDecl {_sdName} -> Just ("var " <> _sdName))

          _ -> pure ()
    collectDecl _ = pure ()

    -- | Tries to find scoped declaration and apply continuation to it or
    -- ignore the declaration if not found.
    withScopedDecl
      :: Range
      -> (ScopedDecl -> WriterT [SymbolInformation] m ())
      -> WriterT [SymbolInformation] m ()
    withScopedDecl r f = maybe (pure ()) f (findScopedDecl r tree)

    -- | Tell to the writer symbol information that we may find in scope or
    -- just ignore it and return `[]`.
    tellScopedDecl
      :: Range
      -> J.SymbolKind
      -> (ScopedDecl -> Maybe Text)
      -> WriterT [SymbolInformation] m ()
    tellScopedDecl range kind' mkName =
      withScopedDecl range $ \sd@ScopedDecl{..} ->
        tell
          [ SymbolInformation
              { _name = fromMaybe _sdName (mkName sd)
              , _deprecated = Nothing
              , _kind = kind'
              , _containerName = matchContainerName kind'
              , _location = J.Location uri $ toLspRange range
              }
          ]

    -- | Tell to the writer some arbitrary symbol info. This is similar to
    -- @tellScopedDecl@ but it does not search for symbol in scope and always
    -- returns non-empty value.
    tellSymbolInfo
      :: Range
      -> J.SymbolKind
      -> Text
      -> WriterT [SymbolInformation] m ()
    tellSymbolInfo range kind' name =
        tell
          [ SymbolInformation
              { _name = name
              , _deprecated = Nothing
              , _kind = kind'
              , _containerName = matchContainerName kind'
              , _location = J.Location uri $ toLspRange range
              }
          ]

    -- | Helper function that associates container name with its container type
    -- defined for the sole purpose of minimising the amount of arguments for `tellScopedDecl`.
    matchContainerName = \case
      J.SkTypeParameter -> Just "type"
      J.SkNamespace -> Just "import"
      J.SkConstant -> Just "const declaration"
      J.SkVariable -> Just "var declaration"
      J.SkFunction -> Just "function"
      _ -> Nothing
