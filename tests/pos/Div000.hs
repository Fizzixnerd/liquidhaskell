module Div000 where

{-@ mydiv :: Int -> {v:Int | v /= 0} -> Int @-}
mydiv :: Int -> Int -> Int
mydiv = undefined

foo :: Int -> Int
foo _ = 12

bar :: Int -> Int
bar m = mydiv m z where z = foo m
