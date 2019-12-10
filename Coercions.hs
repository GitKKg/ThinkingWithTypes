-- | 
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Haskell.ThinkingWithTypes.Coercions where

import Data.Coerce (Coercible(..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Sum (..), Product (..))


newtype Zhen a = Zhen a
za = Zhen 12 :: Zhen Integer
z = coerce za :: Integer -- like force convertion in c, without any runtime cost
-- 12
int = 2 :: Integer
ci = coerce int :: Integer

-- corece would save you from rewriting many functions for your newtype
ap = Product 2
bp = Product 3
mp = ap * bp

as = Sum 2 :: Sum Int
bs = Sum 3 :: Sum Int
ms = as + bs

cs = coerce ms :: Product Int
-- 5

