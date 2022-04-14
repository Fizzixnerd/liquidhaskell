{-# OPTIONS_GHC -fclear-plugins #-}

module Main where

import qualified Language.Stitch.LH.Repl as Repl ( main )

main :: IO ()
main = Repl.main
