{-@ LIQUID "--exact-data-cons" @-}

{-@ type Nat = { i:Int | i >= 0 } @-}
type Nat = Int

f :: Int -> Nat
f x = x

main = print (f 0)
