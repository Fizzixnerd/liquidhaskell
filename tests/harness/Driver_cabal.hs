-- | Test driver for cabal-based builds.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Data.Text (Text)
import qualified Data.Map as M
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Monad (void)
import Test.Build
import Test.Groups
import Test.Summary
import Data.Traversable (for)
import System.Exit (exitSuccess, exitFailure)

default (Text)

testEnv :: Sh ()
testEnv =
  unlessM (test_px "cabal") $ do
    errorExit "Cannot find cabal on the path."

main :: IO ()
main = do
  shelly testEnv
  flagsAndActions <- for (M.toList allTestGroups) $ \(_name, tgd) -> do
    (err, res) <- buildAndParseResults cabalBuild tgd
    let (flag, action) =
          case (err, res) of
            (Left errException, Left resException) ->
              (True, printError errException >> printError resException)
            (Left errException, _) -> (True, printError errException)
            (_, Left resException) -> (True, printError resException)
            (Right err', Right res') ->
              (False, let doc = prettySummarizeResults err' tgd res' in PP.putDoc $ doc PP.<$> PP.empty)
    action
    pure (flag, action)
  putStrLn "\n*** SUMMARY ***"
  -- Redo all the actions in a summary
  void $ traverse snd flagsAndActions
  putStrLn "*** END SUMMARY ***\n"
  if any fst flagsAndActions then do
    putStrLn "Something went wrong, please check the above output."
    exitFailure
  else do
    putStrLn "All tests passed!"
    exitSuccess

  where
    printError :: PP.Pretty a => a -> IO ()
    printError x = PP.putDoc $ PP.pretty x PP.<$> PP.empty
