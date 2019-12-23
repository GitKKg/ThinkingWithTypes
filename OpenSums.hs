-- | 
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module Haskell.ThinkingWithTypes.OpenSums where

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce
import Fcf -- stack install first-class-families

data OpenSum (f :: k -> Type) (ts :: [k]) where -- always remember here is type level
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts -- here is term level
  -- t here is k, t is just existential,not konwn from OpenSum f ts
-- ts could be '[Int, Bool, String], t could be (->) String, k here is just Type too

-- a = Proxy @Int -- :set -XTypeApplications
-- aOpenSum = UnsafeOpenSum  2 "ok" --(Proxy @Int)
aOpenSum = UnsafeOpenSum @Maybe @String 2 (Just "ok")
-- f is Maybe :: * -> *(Type) , k is String
aOpenSum2 = UnsafeOpenSum @Maybe @Int 3 (Just (12 :: Int) )

aOpenSum3 = UnsafeOpenSum @[] @Int 4 [12]
-- f is [] :: * -> * (be able to be Type) , k is Int

-- =<< like fmap
-- return Index or Nothing
type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts
-- FindIndex :: (a -> Exp Bool) -> [a] -> Maybe Nat -> *
-- TyEq :: a -> b -> Bool -> *
-- type Exp a = a -> * , so Exp Bool ~ Bool -> *
-- (b -> Bool -> *) ~ ( b -> Exp Bool)
-- FromMaybe :: k -> Maybe k -> k -> *

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

-- Proxy :: k -> *
tia = Proxy @12
-- k here is 12 in type level!
ia = natVal tia -- 12
-- tia :: Proxy 12
-- natVal :: KnownNat n => proxy n -> Integer -- n of tia here is 12
tsi = Proxy @Int


inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

_12OpenSum = inj @Proxy @12 @'[52,12] Proxy -- @12 
_IntOpenSum = inj @Proxy @Int @'[Int,String]

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts -- use @ in term level to resify abstract kind to concrete type,from type
  then Just $ unsafeCoerce f
  else Nothing

_12prj = prj @Proxy @12 @'[52,12]  _12OpenSum
-- Just Proxy

pp= prj @[] @Int @'[Int] aOpenSum3
-- Nothing
  
decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

--decompose 
