{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Arrow (first)
import Control.Monad (unless)
import Data.Foldable (for_)
import Duplo.Pretty (Pretty, pp, render)
import Main.Utf8 (withUtf8)
import Options.Applicative
  (Parser, ParserInfo, command, execParser, help, helper, hsubparser, info, long, metavar, progDesc,
  short, strOption, switch)

import AST (Fallback, parse, parseWithScopes)
import Cli.Types (HasLigoClient (..))
import ParseTree (Source (Path))

newtype Command = PrintSexp PrintSexpOptions

data PrintSexpOptions = PrintSexpOptions
  { psoContract :: FilePath
  , psoWithScopes :: Bool
  }

commandParser :: Parser Command
commandParser = hsubparser $ mconcat
  [ printSubCommand ]
  where
    printSubCommand = command "print-sexp"
      (info printSexp
        (progDesc "Parse a given contract and print a s-exp representing it."))

printSexp :: Parser Command
printSexp = do
  psoContract <- contractFileOption
  psoWithScopes <- withScopesOption
  pure (PrintSexp PrintSexpOptions{ .. })
  where
    withScopesOption = switch $
      long "with-scopes" <>
      short 's' <>
      help "Whether to add fallback scopes to the tree"

-- | Parser of a path to a contract.
contractFileOption :: Parser FilePath
contractFileOption = strOption $
  long "contract" <>
  metavar "FILEPATH" <>
  help "Path to contract file"

programInfo :: ParserInfo Command
programInfo = info (helper <*> commandParser) mempty

data SomePretty where
  SomePretty :: Pretty a => a -> SomePretty

instance Pretty SomePretty where
  pp (SomePretty a) = pp a

main :: IO ()
main = withUtf8 $
  execParser programInfo >>= \case
    PrintSexp PrintSexpOptions{ .. } -> do
      let parser = if psoWithScopes
            then fmap (first SomePretty) . parseWithScopes @Fallback
            else fmap (first SomePretty) . parse
      (tree, messages) <- parser (Path psoContract)
      putStrLn (render (pp tree))
      unless (null messages) $ do
        putStrLn "The following errors have been encountered: "
        for_ messages $ \(range, err) ->
          putStrLn (render (pp range <> ": " <> pp err))

-- FIXME: HACK
-- We should be able to call `parseWithScopes @Fallback` without the need
-- for a ligo compiler.
instance HasLigoClient IO where
  getLigoClientEnv = error "One does not need ligo for Fallback scopes"
