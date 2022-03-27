-- TAG: termination

module Lex where

bar = foo [1, 2, 3] [2, 3, 4]

{- decrease foo 1 2 @-}
foo xs    (y:ys) = foo xs ys
foo (x:xs) ys    = foo xs ys
foo xs     ys    = xs
