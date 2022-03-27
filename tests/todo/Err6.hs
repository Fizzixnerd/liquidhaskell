-- | Error Message Test: liquid type error

module Err6 where

{-@ zonk :: {v:Int | v = z} @-}
zonk     = (12 :: Int)


