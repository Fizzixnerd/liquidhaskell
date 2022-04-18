-- | Test driver for cabal-based builds.

import Test.Build

main :: IO ()
main = program cabalTestEnv cabalOutputStripper cabalErrorStripper cabalBuild
