{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Build where

import Data.Text (Text)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Text.Megaparsec as P
import Data.Map (Map)
import Test.Types
import Test.Parse hiding (results)
import System.Process.Typed
import Control.Monad (void)
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import System.Environment

-- | Whether or not we want to only build the dependencies of a library (to help
-- sanitize the compiler output)
type OnlyDeps = Bool

-- | Simple wrapper around `readProcess` and `proc` of `System.Process.Typed`.
-- Collects and exit code, stdout, and stderr
command :: Text -> [Text] -> IO (ExitCode, Text, Text)
command cmd args = do
  (ec, out, err) <- readProcess (proc (T.unpack cmd) (T.unpack <$> args))
  pure (ec, toText out, toText err)
  where
    toText = TE.decodeUtf8 . BS.toStrict

-- | Build using cabal, selecting the project file from the
-- `LIQUID_CABAL_PROJECT_FILE` environment variable if possible, otherwise using
-- the default.
cabalBuild :: OnlyDeps -> TestGroupName -> IO (ExitCode, Text, Text)
cabalBuild onlyDeps name = do
  projectFile <- lookupEnv "LIQUID_CABAL_PROJECT_FILE"
  command "cabal" $
    [ "build"
    , "--flag", "build-negs"
    ]
    <> (if onlyDeps then [ "--only-dependencies" ] else [])
    <> (case projectFile of Nothing -> []; Just projectFile' -> [ "--project-file", T.pack projectFile' ])
    <> [ name ]

-- | Build using stack.  XXX Currently doesn't work
stackBuild :: OnlyDeps -> TestGroupName -> IO (ExitCode, Text, Text)
stackBuild onlyDeps name =
  command "stack"
     [ "build"
     , "--flag", "tests:stack"
     , "--flag", "tests:build-negs"
     , (if onlyDeps then "--only-dependencies" else "--")
     , ("tests:" <> name)
     ]

-- | Given a "builder" command and some `TestGroupData`, create an IO action
-- that parses the results of running the build command. Outputs can be fed into
-- functions in `Summary.hs`.
buildAndParseResults
  :: (OnlyDeps -> TestGroupName -> IO (ExitCode, Text, Text))
  -> TestGroupData
  -> IO (Either ErrorException (Map (Maybe ModuleName) [CompilerMessage]), Either ResultsException [ModuleInfo])
buildAndParseResults builder tgd@TestGroupData {..} = do
  T.putStrLn $ "Ensuring dependencies for " <> tgdName <> " are up to date..."
  void $ builder True tgdName
  T.putStrLn $ "Building " <> tgdName <> " for real now!"
  (_, out', err') <- builder False tgdName
  let out = stripAnsiEscapeCodes out'
      err = stripDDumpTimingsOutput . stripAnsiEscapeCodes $ err'
  T.putStrLn out
  T.putStrLn $ "*** STDERR " <> tgdName <> " ***\n" <> err <> "\n*** END STDERR " <> tgdName <> " ***\n"
  errMap <-
    either
      ((>> (pure $ Left $ FishyErrorParseException tgdName)) . printError)
      pure
      $ parseErrors tgd err
  results <-
    either
      ((>> (pure $ Left $ FishyResultsParseException tgdName)) . printError)
      pure
      $ parseResults tgd out
  pure (errMap, results)
  where
    printError = T.putStrLn . T.pack . P.errorBundlePretty
