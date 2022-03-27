{-# LANGUAGE DataKinds #-}

module DataKinds where

import           Data.Proxy

-- TODO-REBARE: The following works ...
{- sizeOfMember :: _ -> Nat @-}

-- TODO-REBARE: ... but this does not. 
{-@ sizeOfMember :: Proxy a -> Nat @-}

sizeOfMember :: Proxy a -> Int
sizeOfMember = undefined 
