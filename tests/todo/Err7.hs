-- | Error Message Test: liquid type error

module Err7 where

{-@ tonk :: {v:Int | (Prop v) = v } @-}
tonk     = (12 :: Int)

