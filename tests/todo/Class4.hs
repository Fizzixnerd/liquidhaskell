module Class4 where

{-@ class Pos s where
      allPos :: s a -> Bool
  @-}
class Pos s where
  allPos :: s a -> Bool
