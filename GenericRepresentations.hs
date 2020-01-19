{-# LANGUAGE TypeFamilies #-}
-- | 
{-# LANGUAGE DeriveGeneric #-}

module Haskell.ThinkingWithTypes.GenericRepresentations where

import GHC.Types

class Generic a where
  type Rep a :: Type -> Type -- type : TypeFamilies must be enabled
  from :: a -> Rep a x
  -- given a variable of type a,then get a function convert this variable of type a to type x
  to :: Rep a x -> a
