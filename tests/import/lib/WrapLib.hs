module WrapLib where

import WrapLibCode

{-@ assume WrapLibCode.foo :: x:Nat -> {v:Nat | v = x + 1}  @-}

