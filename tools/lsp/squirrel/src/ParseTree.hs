{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StrictData, TupleSections #-}

{- | The input tree from TreeSitter. Doesn't have any pointers to any data
     from actual tree the TS produced and therefore has no usage limitations.

     All datatypes here are strict.
-}

module ParseTree
  ( -- * Tree/Forest
    ParseTree(..)
  , RawInfo
  , RawTree
  , SomeRawTree(..)
  , Source(..)

    -- * Invoke the TreeSitter and get the tree it outputs
  , toParseTree

    -- * Read file contents from its source
  , srcToBytestring
  , srcToText
  )
  where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Traversable (for)

import Control.Monad ((>=>))
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import TreeSitter.Language
import TreeSitter.Node
import TreeSitter.Parser
import TreeSitter.Tree hiding (Tree)

import System.FilePath (takeFileName)

import Duplo.Pretty as PP
import Duplo.Tree

import Extension
import qualified Log
import Product
import Range

foreign import ccall unsafe tree_sitter_PascaLigo  :: Ptr Language
foreign import ccall unsafe tree_sitter_ReasonLigo :: Ptr Language
foreign import ccall unsafe tree_sitter_CameLigo   :: Ptr Language

data Source
  = Path       { srcPath :: FilePath }
  | Text       { srcPath :: FilePath, srcText :: Text }
  | ByteString { srcPath :: FilePath, srcBS   :: ByteString }

instance IsString Source where
  fromString = Path

instance Show Source where
  show = show . srcPath

srcToBytestring :: Source -> IO ByteString
srcToBytestring = \case
  Path       p   -> BS.readFile p
  Text       _ t -> return $ Text.encodeUtf8 t
  ByteString _ s -> return s

srcToText :: Source -> IO Text
srcToText = \case
  Path       p   -> Text.readFile p
  Text       _ t -> return t
  ByteString _ s -> return $ Text.decodeUtf8 s

data SomeRawTree where
  SomeRawTree
    :: Lang -> RawTree -> SomeRawTree

type RawTree = Tree '[ParseTree] RawInfo
type RawInfo = Product [Range, Text]

-- instance {-# OVERLAPS #-} Modifies RawInfo where
--   ascribe (r :> n :> _) d = color 3 (pp n) `indent` pp d

data TreeKind
  = Error
  | Comment
  | Field Text
  deriving stock (Eq, Ord)

-- TODO: move and refactor
instance (Pretty v) => Pretty (Map k v) where
  pp = pp . fmap snd . toList

instance Pretty TreeKind where
  pp = \case
    Error -> "error"
    Comment -> "comment"
    Field t -> "field (" PP.<.> pp t PP.<.> ")"

-- | The tree tree-sitter produces.
data ParseTree self = ParseTree
  { ptName     :: Text         -- ^ Name of the node.
  , ptChildren :: [self]       -- ^ Subtrees.
  , ptSource   :: ~Text        -- ^ Range of the node.
  }
  deriving stock (Functor, Foldable, Traversable)

instance Pretty1 ParseTree where
  pp1 (ParseTree n forest _) =
    parens
      ( hang
        (quotes (text (Text.unpack n)))
        2
        (pp forest)
      )

-- | Feed file contents into PascaLIGO grammar recogniser.
toParseTree :: Source -> IO SomeRawTree
toParseTree input = do
  Log.debug "TS" [Log.i|Reading #{input}|]
  (language, dialect) <- onExt ElimExt
    { eePascal = (tree_sitter_PascaLigo, Pascal)
    , eeCaml   = (tree_sitter_CameLigo, Caml)
    , eeReason = (tree_sitter_ReasonLigo, Reason)
    } (srcPath input)

  SomeRawTree dialect <$> withParser language \parser -> do
    src <- srcToBytestring input
    res <- withParseTree parser src \tree -> do
      withRootNode tree (peek >=> go input src)
    Log.debug "TS" [Log.i|Done reading #{input}|]
    return res

  where
    go :: Source -> ByteString -> Node -> IO RawTree
    go fin src node = do
      let count = fromIntegral $ nodeChildCount node
      allocaArray count $ \children -> do
        alloca $ \tsNodePtr -> do
          poke tsNodePtr $ nodeTSNode node
          ts_node_copy_child_nodes tsNodePtr children
          nodes <- for [0.. count - 1] $ \i -> do
            peekElemOff children i

          trees <- for nodes \node' -> do
            (only -> (r :> _, tree :: ParseTree RawTree)) <- go fin src node'
            field <-
              if   nodeFieldName node' == nullPtr
              then return ""
              else peekCString $ nodeFieldName node'
            return $ make (r :> Text.pack field :> Nil, tree)

          ty <- peekCString $ nodeType node

          let
            start2D  = nodeStartPoint node
            finish2D = nodeEndPoint   node
            -- An empty node indicates a missing token, for example, if we have:
            -- `function idsa (const iff : int) : int is (iff`
            -- Then tree-sitter will report:
            -- `(MISSING ")" [0, 45] - [0, 45])`
            -- But won't indicate an error on the parse tree itself. According to
            -- https://github.com/tree-sitter/tree-sitter-bash/issues/27#issuecomment-410865045
            -- we can check for this by testing whether we have an empty node.
            name     = if start2D == finish2D then "ERROR" else Text.pack ty
            i        = fromIntegral

          let
            range = Range
              { rStart  =
                  ( i $ pointRow    start2D + 1
                  , i $ pointColumn start2D + 1
                  , i $ nodeStartByte node
                  )

              , rFinish =
                  ( i $ pointRow    finish2D + 1
                  , i $ pointColumn finish2D + 1
                  , i $ nodeEndByte node
                  )
              , rFile = takeFileName $ srcPath fin
              }

          return $ make
            ( range :> "" :> Nil
            , ParseTree
              { ptName     = name
              , ptChildren = trees
              , ptSource   = cutOut range src
              }
            )
