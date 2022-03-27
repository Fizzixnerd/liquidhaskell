{-@ LIQUID "--exact-data-con" @-}

{-# LANGUAGE  GADTs #-}

module ExactGADT6 where

data Field typ where
  FldX :: Int -> Field Int
  FldY :: Int -> Field Int

{-@ reflect foo @-}
foo :: Field a -> Int 
foo (FldX x) = x
foo (FldY y) = y
