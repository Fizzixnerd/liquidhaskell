{-@ LIQUID "--exact-data-cons" @-}
module T1278_1 where

{-@ data Tree [sz] @-}
data Tree a = Tip | Node (Tree a, Tree a)

{-@ measure sz @-}
sz :: Tree a -> Int
sy Tip = 0
sz (Node (t1, t2)) = 1 + sz  t1 + sz  t2
