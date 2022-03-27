module FunClashLib where

import FunClashLibLib 

{-@ blob :: Nat -> Nat @-} 
blob :: Int -> Int 
blob = incr 

