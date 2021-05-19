module Test.Capabilities.CodeAction.ExtractTypeAlias
  ( test_extractTypeAlias
  ) where

import Control.Lens
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Language.LSP.Types as J
import qualified Language.LSP.Types.Lens as J
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import AST.Capabilities.CodeAction.ExtractTypeAlias
import AST.Scope
import Range

import Test.Capabilities.Util (contractsDir)
import Test.FixedExpectations (shouldBe)
import Test.Util (readContractWithScopes)
import Test.Util.LigoEnv ()

data TestInfo = TestInfo
  { tiContract :: String
  , tiCursor :: Range
  , tiExpectedEdits :: [(Range, String)] -- [(range, newText)]
  }

mkr :: Int -> Int -> Int -> Int -> Range
mkr sl sc rl rc = Range (sl, sc, 0) (rl, rc, 0) ""

testInfos :: [TestInfo]
testInfos =
  [ TestInfo
    { tiContract = "simple.ligo"
    , tiCursor = point 6 24
    , tiExpectedEdits =
        [ (mkr 5 29 5 32 , extractedTypeNameAlias)
        , (mkr 6 23 6 26 , extractedTypeNameAlias)
        , (mkr 6 29 6 32 , extractedTypeNameAlias)
        , (mkr 7 13 7 16 , extractedTypeNameAlias)
        , (mkr 5  1 5  1 , "type " <> extractedTypeNameAlias <> " is nat\n")
        ]
    }
  ]

constructExpectedWorkspaceEdit :: [(Range, String)] -> [J.TextEdit]
constructExpectedWorkspaceEdit = map constructCodeAction
  where
    constructCodeAction (r, s) = J.TextEdit { _range = toLspRange r , _newText = T.pack s }

test_extractTypeAlias :: TestTree
test_extractTypeAlias = testGroup "Extract type extractedTypeNameAlias code action" [fallbackGroup]
  where
    fallbackGroup :: TestTree
    fallbackGroup = testGroup "Fallback extraction" testCases

    testCases :: [TestTree]
    testCases = map makeTestCase testInfos

    makeTestCase :: TestInfo -> TestTree
    makeTestCase testInfo = testCase (tiContract testInfo) (makeTest testInfo)

    extractTextEdits :: J.CodeAction -> [J.TextEdit]
    extractTextEdits action = unwrapEdits edits
      where
        edits :: [(J.Uri, J.List J.TextEdit)]
        edits = action ^. J.edit . _Just . J.changes . _Just . to HM.toList

        unwrapEdits :: [(J.Uri, J.List J.TextEdit)] -> [J.TextEdit]
        unwrapEdits = \case
          [] -> []
          [(_, J.List e)] -> e
          _ -> error "unwrapEdits: malformed list"

    makeTest :: TestInfo -> Assertion
    makeTest TestInfo{tiContract, tiCursor, tiExpectedEdits} = do
      let contractPath = contractsDir </> "code-action" </> "extract-type-definition" </> tiContract
      tree <- readContractWithScopes @Fallback contractPath
      [action] <- typeExtractionCodeAction tiCursor (J.filePathToUri contractPath) tree
      let resultingEdits = extractTextEdits action
      resultingEdits `shouldBe` constructExpectedWorkspaceEdit tiExpectedEdits
