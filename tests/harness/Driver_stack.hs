{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Test.Types
import Test.Build
import Test.Groups
import Test.Summary
import Test.Parse

default (Text)

testEnv :: Sh ()
testEnv =
  unlessM (test_px "stack") $ do
    errorExit "Cannot find stack on the path."

main :: IO ()
main = shelly $ do
  testEnv
  results <- traverse (\name -> traverse (buildAndParseResults stackBuild skipStackLineHeader) (name, name)) $ M.keys allTestGroups
  echo . T.pack . show $ (\(name, mRes) -> summarizeResults name <$> mRes) <$> results
