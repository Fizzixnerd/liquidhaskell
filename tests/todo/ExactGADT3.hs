{-@ LIQUID "--exact-data-con" @-}

{-# LANGUAGE  GADTs #-}

module ExactGADT3 where

data Field typ where
  FldX :: Field Int
  FldY :: Field Int

{-@ reflect foo @-}
foo :: Field a -> Int 
foo FldX = 10 
foo FldY = 21
