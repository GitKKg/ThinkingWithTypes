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
  format :: String -> Proxy a -> Printf a

-- some analogy for class
-- class Show a where
--   showsPrec :: Int -> a -> ShowS
--   show :: a -> String
--   showList :: [a] -> ShowS
--   {-# MINIMAL showsPrec | show #-}

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text)
-- "!" is text

instance (KnownSymbol text , HasPrintf a) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)
-- (":" :<< Bool :<< "!")  ":" is text, Bool :<< "!" is a

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s <> show param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""


-- (Int :<< ":" :<< Bool :<< "!") Int is param, ":" :<< Bool :<< "!" is a

-- some analogy
type ZhenShu = Int
type MayInt = Maybe Int
type May a = Maybe a

-- test in lanmbda command line
-- :set -XDataKinds
-- :set -XTypeOperators
-- :kind! Printf ( Int :<< ":" :<< Bool :<< "!")
-- Printf ( Int :<< ":" :<< Bool :<< "!") :: *
-- = Int -> Bool -> String

-- :set -XTypeApplications
-- printf ( Proxy @( Int :<< "+" :<< Int :<< "=3") ) 1 2
-- "1+2=3"
