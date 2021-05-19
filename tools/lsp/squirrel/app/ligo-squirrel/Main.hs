{-# LANGUAGE PolyKinds #-}

import Control.Exception.Safe (MonadCatch, catchAny, displayException)
import Control.Lens hiding ((:>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T

import qualified Language.LSP.Server as S
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J


import AST
import qualified ASTMap
import qualified Config
import Language.LSP.Util (sendError)
import qualified Log
import Product
import RIO (RIO, RioEnv)
import qualified RIO
import Range
import System.Exit
import qualified System.Log as L


main :: IO ()
main = do
  Log.setLogLevel Log.ERROR
  exit =<< mainLoop

mainLoop :: IO Int
mainLoop = do
    let
      serverDefinition = S.ServerDefinition
        { S.onConfigurationChange = Config.getConfigFromNotification
        , S.defaultConfig = def
        , S.doInitialize = \lcEnv _msg -> Right . (lcEnv, ) <$> initialize
        , S.staticHandlers = catchExceptions handlers
        , S.interpretHandler = \envs -> S.Iso (RIO.run envs) liftIO
        , S.options = lspOptions
        }

    S.setupLogger Nothing [] L.EMERGENCY
    S.runServer serverDefinition
  where
    syncOptions :: J.TextDocumentSyncOptions
    syncOptions = J.TextDocumentSyncOptions
      { J._openClose         = Just True
      , J._change            = Just J.TdSyncIncremental
      , J._willSave          = Just False
      , J._willSaveWaitUntil = Just False
      , J._save              = Just $ J.InR $ J.SaveOptions $ Just False
      }

    lspOptions :: S.Options
    lspOptions = def
      { S.textDocumentSync       = Just syncOptions
      , S.executeCommandCommands = Just ["lsp-hello-command"]
      , S.signatureHelpTriggerCharacters = Just ['(', ' ']
      , S.signatureHelpRetriggerCharacters = Just [',']
      }


    -- | Handle all uncaught exceptions.
    catchExceptions
      :: forall m config. (MonadCatch m, S.MonadLsp config m)
      => S.Handlers m -> S.Handlers m
    catchExceptions = S.mapHandlers wrapReq wrapNotif
      where
        wrapReq
          :: forall (meth :: J.Method 'J.FromClient 'J.Request).
             S.Handler m meth -> S.Handler m meth
        wrapReq handler msg@J.RequestMessage{_method} resp =
          handler msg resp `catchAny` \e -> do
            Log.err "Uncaught" $ "Handling `" <> show _method <> "`: " <> displayException e
            resp . Left $ J.ResponseError J.InternalError (T.pack $ displayException e) Nothing

        wrapNotif
          :: forall (meth :: J.Method 'J.FromClient 'J.Notification).
             S.Handler m meth -> S.Handler m meth
        wrapNotif handler msg@J.NotificationMessage{_method} =
          handler msg `catchAny` \e -> do
            Log.err "Uncaught" $ "Handling `" <> show _method <> "`: " <> displayException e
            sendError . T.pack $ "Error handling `" <> show _method <> "` (see logs)."


initialize :: IO RioEnv
initialize = do
    astMap <- ASTMap.empty $ RIO.load . J.fromNormalizedUri
    pure (astMap :> Nil)

handlers :: S.Handlers RIO
handlers = mconcat
  [ S.notificationHandler J.SInitialized (\_msg -> pure ())

  , S.notificationHandler J.STextDocumentDidOpen handleDidOpenTextDocument
  , S.notificationHandler J.STextDocumentDidChange handleDidChangeTextDocument
  , S.notificationHandler J.STextDocumentDidSave (\_msg -> pure ())

  , S.requestHandler J.STextDocumentDefinition handleDefinitionRequest
  , S.requestHandler J.STextDocumentTypeDefinition handleTypeDefinitionRequest
  , S.requestHandler J.STextDocumentReferences handleFindReferencesRequest
  , S.requestHandler J.STextDocumentCompletion handleCompletionRequest
  --, S.requestHandler J.SCompletionItemResolve handleCompletionItemResolveRequest
  , S.requestHandler J.STextDocumentSignatureHelp handleSignatureHelpRequest
  , S.requestHandler J.STextDocumentFoldingRange handleFoldingRangeRequest
  , S.requestHandler J.STextDocumentSelectionRange handleSelectionRangeRequest
  , S.requestHandler J.STextDocumentDocumentSymbol handleDocumentSymbolsRequest
  , S.requestHandler J.STextDocumentHover handleHoverRequest
  , S.requestHandler J.STextDocumentRename handleRenameRequest
  , S.requestHandler J.STextDocumentPrepareRename handlePrepareRenameRequest
  , S.requestHandler J.STextDocumentFormatting handleDocumentFormattingRequest
  , S.requestHandler J.STextDocumentRangeFormatting handleDocumentRangeFormattingRequest
  , S.requestHandler J.STextDocumentCodeAction handleTextDocumentCodeAction
  -- , S.requestHandler J.STextDocumentOnTypeFormatting

  , S.notificationHandler J.SCancelRequest (\_msg -> pure ())
  --, S.requestHandler J.STextDocumentCodeAction _
  --, S.requestHandler J.SWorkspaceExecuteCommand _
  ]

handleDidOpenTextDocument :: S.Handler RIO 'J.TextDocumentDidOpen
handleDidOpenTextDocument notif = do
  let doc = notif^.J.params.J.textDocument.J.uri
  let ver = notif^.J.params.J.textDocument.J.version
  RIO.collectErrors RIO.forceFetch (J.toNormalizedUri doc) (Just ver)

handleDidChangeTextDocument :: S.Handler RIO 'J.TextDocumentDidChange
handleDidChangeTextDocument notif = do
  tmap <- asks getElem
  let uri = notif^.J.params.J.textDocument.J.uri.to J.toNormalizedUri
  ASTMap.invalidate uri tmap
  RIO.collectErrors (`ASTMap.fetchBundled` tmap) uri (Just 0)

handleDefinitionRequest :: S.Handler RIO 'J.TextDocumentDefinition
handleDefinitionRequest req respond = do
    -- XXX: They forgot lenses for DefinitionParams :/
    {-
    let uri = req^.J.textDocument.J.uri
    let pos = fromLspPosition $ req^.J.position
    -}
    let
      J.DefinitionParams{_textDocument, _position} = req ^. J.params
      uri = _textDocument ^. J.uri
      pos = fromLspPosition _position
    (tree, _) <- RIO.fetch $ J.toNormalizedUri uri
    case AST.definitionOf pos tree of
      Just defPos -> respond . Right . J.InR . J.InL . J.List $ [J.Location uri $ toLspRange defPos]
      Nothing     -> respond . Right . J.InR . J.InL . J.List $ []

handleTypeDefinitionRequest :: S.Handler RIO 'J.TextDocumentTypeDefinition
handleTypeDefinitionRequest req respond = do
    let
      J.TypeDefinitionParams{_textDocument, _position} = req ^. J.params
      uri = _textDocument ^. J.uri
      pos = _position ^. to fromLspPosition
    (tree, _) <- RIO.fetch $ J.toNormalizedUri uri
    let wrapAndRespond = respond . Right . J.InR . J.InL . J.List
    case AST.typeDefinitionAt pos tree of
      Just defPos -> wrapAndRespond [J.Location uri $ toLspRange defPos]
      Nothing     -> wrapAndRespond []

handleDocumentFormattingRequest :: S.Handler RIO 'J.TextDocumentFormatting
handleDocumentFormattingRequest req respond = do
  let
    uri = req ^. J.params . J.textDocument . J.uri
  (tree, _) <- RIO.fetch $ J.toNormalizedUri uri
  respond . Right =<< AST.formatDocument tree

handleDocumentRangeFormattingRequest :: S.Handler RIO 'J.TextDocumentRangeFormatting
handleDocumentRangeFormattingRequest req respond = do
  let
    uri = req ^. J.params . J.textDocument . J.uri
    pos = fromLspRange $ req ^. J.params . J.range
  (tree, _) <- RIO.fetch $ J.toNormalizedUri uri
  respond . Right =<< AST.formatAt pos tree

handleFindReferencesRequest :: S.Handler RIO 'J.TextDocumentReferences
handleFindReferencesRequest req respond = do
    let (uri, nuri, pos) = getUriPos req
    (tree, _) <- RIO.fetch nuri
    case AST.referencesOf pos tree of
      Just refs -> do
        let locations = J.Location uri . toLspRange <$> refs
        respond . Right . J.List $ locations
      Nothing ->
        respond . Right . J.List $ []

handleCompletionRequest :: S.Handler RIO 'J.TextDocumentCompletion
handleCompletionRequest req respond = do
    Log.debug "Completion" [i|Request: #{show req}|]
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let pos = fromLspPosition $ req ^. J.params . J.position
    (tree, _) <- RIO.fetch uri
    let completions = fmap toCompletionItem . fromMaybe [] $ complete pos tree
    respond . Right . J.InL . J.List $ completions

{-
handleCompletionItemResolveRequest :: S.Handler RIO 'J.CompletionItemResolve
handleCompletionItemResolveRequest req respond = do
    Log.debug "Completion resolve" [i|Request: #{show req}|]
    respond . Right $  req ^. J.params
-}

handleSignatureHelpRequest :: S.Handler RIO 'J.TextDocumentSignatureHelp
handleSignatureHelpRequest req respond = do
  -- XXX: They forgot lenses for  SignatureHelpParams :/
  {-
  let uri = req ^. J.params . J.textDocument . J.uri
  let position = req ^. J.params . J.position & fromLspPosition
  -}
  let
    J.SignatureHelpParams{_textDocument, _position} = req ^. J.params
    uri = _textDocument ^. J.uri
    position = fromLspPosition _position
  (tree, _) <- RIO.fetch (J.toNormalizedUri uri)
  let signatureHelp = getSignatureHelp (tree ^. nestedLIGO) position
  respond . Right $ signatureHelp

handleFoldingRangeRequest :: S.Handler RIO 'J.TextDocumentFoldingRange
handleFoldingRangeRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    (tree, _) <- RIO.fetch uri
    actions <- foldingAST (tree ^. nestedLIGO)
    respond . Right . J.List $ toFoldingRange <$> actions

handleTextDocumentCodeAction :: S.Handler RIO 'J.TextDocumentCodeAction
handleTextDocumentCodeAction req respond = do
    let
      uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
      r = req ^. J.params . J.range . to fromLspRange
      con = req ^. J.params . J.context
    (tree, _) <- RIO.fetch uri
    actions <- collectCodeActions r con (J.fromNormalizedUri uri) tree
    let response = Right . J.List . fmap J.InR $ actions
    respond response

handleSelectionRangeRequest :: S.Handler RIO 'J.TextDocumentSelectionRange
handleSelectionRangeRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let positions = req ^. J.params . J.positions ^.. folded
    (tree, _) <- RIO.fetch uri
    let results = map (findSelectionRange (tree ^. nestedLIGO)) positions
    respond . Right . J.List $ results

handleDocumentSymbolsRequest :: S.Handler RIO 'J.TextDocumentDocumentSymbol
handleDocumentSymbolsRequest req respond = do
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    (tree, _) <- RIO.fetch uri
    result <- extractDocumentSymbols (J.fromNormalizedUri uri) tree
    respond . Right . J.InR . J.List $ result

handleHoverRequest :: S.Handler RIO 'J.TextDocumentHover
handleHoverRequest req respond = do
    -- XXX: They forgot lenses for  HoverParams :/
    {-
    let uri = req ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
    let pos = fromLspPosition $ req ^. J.params . J.position
    -}
    let
      J.HoverParams{_textDocument, _position} = req ^. J.params
      uri = _textDocument ^. J.uri . to J.toNormalizedUri
      pos = fromLspPosition _position
    (tree, _) <- RIO.fetch uri
    respond . Right $ hoverDecl pos tree

handleRenameRequest :: S.Handler RIO 'J.TextDocumentRename
handleRenameRequest req respond = do
    let (uri, nuri, pos) = getUriPos req
    let newName = req ^. J.params . J.newName

    (tree, _) <- RIO.fetch nuri

    case renameDeclarationAt pos tree newName of
      NotFound -> do
        Log.debug "Rename" [i|Declaration not found for: #{show req}|]
        respond . Left $
          J.ResponseError J.InvalidRequest "Cannot rename this" Nothing
      Ok edits ->
        let
          -- TODO: Add support for renaming references in other files.
          changes = HM.fromList [ (uri, J.List edits ) ]

          -- XXX: This interface has two benefits: it allows to refer to a specific
          -- document version and it allows the creation/deletion/renaming of files.
          -- In this case we do not care about the latter and the actual usefulness
          -- of the former is not clear either, but it might be worth switching
          -- to it when we support verions.
          --documentChanges = J.List
          --  [ J.TextDocumentEdit
          --      { _textDocument = J.VersionedTextDocumentIdentifier uri Nothing
          --      , _edits = J.List edits
          --      }
          --  ]

          response =
            J.WorkspaceEdit
              { _changes = Just changes
              , _documentChanges = Nothing
              }
        in respond . Right $ response

handlePrepareRenameRequest :: S.Handler RIO 'J.TextDocumentPrepareRename
handlePrepareRenameRequest req respond = do
    let (_, nuri, pos) = getUriPos req

    (tree, _) <- RIO.fetch nuri

    respond . Right . fmap (J.InL . toLspRange) $ prepareRenameDeclarationAt pos tree

getUriPos
  :: ( J.HasPosition (J.MessageParams m) J.Position
     , J.HasUri a J.Uri
     , J.HasTextDocument (J.MessageParams m) a
     )
  => J.RequestMessage m
  -> (J.Uri, J.NormalizedUri, Range)
getUriPos req =
  let
    uri  = req ^. J.params . J.textDocument . J.uri
    nuri = J.toNormalizedUri uri
    pos  = fromLspPosition $ req ^. J.params . J.position
  in (uri, nuri, pos)

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith (ExitFailure n)
