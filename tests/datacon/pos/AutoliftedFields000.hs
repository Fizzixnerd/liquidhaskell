{-@ LIQUID "--exact-data-cons" @-}

-- data decl in LH is missing but uses a LH-refined type alias correctly

module AutoliftedFields000 where

{-@ type Nat = { v : Int | v >= 0 } @-}
type Nat = Int

{-@ data T = T { getT :: Nat } @-}
data T = T { getT :: Nat }

{-@ f :: Int -> Nat @-}
f :: Int -> Nat
f = getT . T

g :: Int -> Nat
g z = getT (T z)

x :: T
x = T 0

y :: Nat
y = getT x
