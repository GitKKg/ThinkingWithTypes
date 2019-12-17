{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
-- | 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskell.ThinkingWithTypes.BuidldingTypesFromSchema where

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import GHC.TypeLits

import Data.Typeable

data (a :: k1) :<< (b :: k2)
  --where
    --La :: a -> a :<< b -- La is data constructor
infixr 5 :<<

-- Just analogy, to show Type constructor is :<< , and ::<<
data (::<<) (a :: k1) (b :: k2)
infixr 5 ::<<

data LinkD where
  LinkS :: h -> LinkD
  Link :: h -> LinkD -> LinkD
infixr 5 `Link`

hLength :: LinkD -> Int
hLength (LinkS h) = 0
hLength (_ `Link` ts) = 1 + hLength ts

--type family HeadL (lts :: LinkD) :: h where
--  HeadL (h `Link` ts) = h

la = LinkS 12
lb = [190] `Link` "ok" `Link` la

tailL (h `Link` hs) = hs -- pass,for hs is always of type LinkD here
-- not pass,because h is not a fixed type here, need some other tech
-- headL (LinkS h) = h

class HasPrintf a where
  type Printf a :: Type -- every instance must provide an associated type Printf a

instance HasPrintf (text :: Symbol) where
  type Printf text = String

instance HasPrintf a => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a

instance HasPrintf a => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a

-- some analogy
type ZhenShu = Int
type MayInt = Maybe Int
type May a = Maybe a
