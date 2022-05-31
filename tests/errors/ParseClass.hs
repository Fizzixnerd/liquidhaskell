-- This fails because parse errors cannot be caught by --expect-error-containing
{-@ LIQUID "--expect-error-containing=Cannot parse specification" @-}
module ParseClass where

class Foo a where
{-@ class Foo where
      foo :: x:a -> {v:a | v = x}
  @-}
  foo :: a -> a       

instance Foo Int where
  {-@ instance Foo Int where
       foo :: x:Int -> {v:Int | v = x + 1 == 9} @-}
  foo x = x + 1

instance Foo Integer where
  {-@ instance Foo Integer where
       foo :: x:Integer -> {v:Integer | v = x + 1} @-}
  foo x = x + 1
