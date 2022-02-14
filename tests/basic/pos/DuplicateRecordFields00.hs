{-# LANGUAGE DuplicateRecordFields #-}

module DuplicateRecordFields00 where

data TS = S { x :: Int }
        | T { x :: Int }

y :: TS
y = S 0

z :: TS
z = T 0

f :: Int
f = x y + x z
