
--
-- Liquidate your assets: reasoning about resource usage in Liquid Haskell.
-- Martin A.T. Handley, Niki Vazou, and Graham Hutton.
--

{-@ LIQUID "--reflection" @-}

module Language.Haskell.Liquid.RTick
  ()
{-
  -- Tick datatype:
    Tick(..)
  -- Primitive resource operators:
  , fmap
  , pure
  , (<*>)
  , liftA2
  , return
  , (>>=)
  , (=<<)
  , eqBind
  , leqBind
  , geqBind
  , ap
  , liftM
  , liftM2
  -- Resource modifiers:
  , step
  , wait      -- step 1       . return
  , waitN     -- step (n > 0) . return
  , go        -- step (-1)    . return
  , goN       -- step (n < 0) . return
  , wmap      -- step 1       . fmap f
  , wmapN     -- step (n > 0) . fmap f
  , gmap      -- step (-1)    . fmap f
  , gmapN     -- step (n < 0) . fmap f
  , (</>)     -- step 1       . (f <*>)
  , (<//>)    -- step 2       . (f <*>)
  , (<\>)     -- step (-1)    . (f <*>)
  , (<\\>)    -- step (-2)    . (f <*>)
  , (>/=)     -- step 1       . (>>= f)
  , (=/<)     -- step 1       . (>>= f)
  , (>//=)    -- step 2       . (>>= f)
  , (=//<)    -- step 2       . (>>= f)
  , (>\=)     -- step (-1)    . (>>= f)
  , (=\<)     -- step (-1)    . (>>= f)
  , (>\\=)    -- step (-2)    . (>>= f)
  , (=\\<)    -- step (-2)    . (>>= f)
  -- Memoisation:
  , pay
  , zipWithM


  )-} where

import Prelude hiding ( Functor(..), Applicative(..), Monad(..), (=<<) )

import qualified Control.Applicative as A
import qualified Control.Monad       as M
import qualified Data.Functor        as F

--
-- The 'Tick' datatype and its corresponding resource modifiers.
--
-- See 'ResourceModifiers.hs' for proofs that all resource modifiers
-- can be defined using 'return', '(>>=) 'and 'step'.
--

