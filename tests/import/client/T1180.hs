{-# LANGUAGE GADTs #-}

{-@ LIQUID "--exact-data-con" @-}
{-@ LIQUID "--noadt" @-}

module T1180 where

import Peano 

pInt :: Peano -> Int 
pInt Z     = 0 
pInt (S n) = 1 + pInt n
