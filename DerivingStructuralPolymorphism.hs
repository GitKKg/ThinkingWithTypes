{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
-- | 
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Haskell.ThinkingWithTypes.DerivingStructuralPolymorphism where

import GHC.Generics
import GHC.Types

class GEq a where
  geq :: a x -> a x -> Bool

-- :k U1
-- U1 :: k -> *
instance GEq U1 where -- U1 represents a data constructor with no parameters, just () with a different name, just like a Unit type
  geq U1 U1 = True
-- :t U1
-- U1  :: U1 p

-- :k V1
-- V1 :: k -> *
instance GEq V1 where -- V1 corresponds to types can't be constructed, jus like Void,the type with no inhabitants
  geq _ _ = True
-- since V1 can't be constructed , so can't get :t V1 

-- just how the Eq instance for Maybe a is Eq a => Eq (Maybe a)
-- :k K1
-- K1 :: * -> * -> k -> *
instance Eq a => GEq (K1  _1  a) where
-- _1 here is * -> * ? just like Maybe :: * -> *,no ,should be  first *,1 here just a number for refernce ,not order, see P57 ,showBool function explaining
-- K1 _1 a is type (* -> *), just GEq :: (* -> *) -> Constraint wants
  geq (K1  a) (K1  b) = a == b
-- :t K1
-- K1 :: c -> K1 i c p
-- :i K1
-- type role K1 phantom representational phantom
-- newtype K1 i c (p :: k) = K1 {unK1 :: c}

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a1) (L1 a2) = geq a1 a2 -- L1 :: f p -> (:+:) f g p
  geq (R1 b1) (R1 b2) = geq b1 b2 -- R1 :: g p -> (:+:) f g p
  geq _ _ = False
-- :k (:+:)
-- (:+:) :: (k -> *) -> (k -> *) -> k -> *
-- :i L1
-- type role (:+:) representational representational nominal
-- data (:+:) (f :: k -> *) (g :: k -> *) (p :: k) = L1 (f p) | ...
-- :i R1
-- data (:+:) (f :: k -> *) (g :: k -> *) (p :: k) = ... | R1 (g p)
-- :i (:+:)
-- type role (:+:) representational representational nominal
-- data (:+:) (f :: k -> *) (g :: k -> *) (p :: k)
--   = L1 (f p) | R1 (g p)
--   	-- Defined in ‘GHC.Generics’
-- infixr 5 :+:

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2
-- :i (:*:)
-- type role (:*:) representational representational nominal
-- data (:*:) (f :: k -> *) (g :: k -> *) (p :: k) = (f p) :*: (g p)
--   	-- Defined in ‘GHC.Generics’
-- infixr 6 :*:

instance GEq a => GEq (M1 _x _y a) where -- _X _2 is ok too, x is ok too
  geq (M1 a1) (M1 a2) = geq a1 a2
-- :i M1
-- type role M1 phantom phantom representational nominal
-- newtype M1 i (c :: Meta) (f :: k -> *) (p :: k) = M1 {unM1 :: f p}

genericEq :: (Generic a,GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

data Foo a b c
  = F0
  | F1 a
  | F2 b c
  deriving (Generic)

instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
  (==) = genericEq -- let ghi draw tiger accoiding to cat

class GOrd a where
  glq :: a x -> a x -> Bool
  gbq :: a x -> a x -> Bool

instance GOrd U1 where
  glq U1 U1 = True
  gbq U1 U1 = True

instance Ord a => GOrd (K1 _1 a) where
  glq (K1 a) (K1 b) = a <=b
  gbq (K1 a) (K1 b) = a >=b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
  glq (L1 a1) (L1 a2) = glq a1 a2
  glq (R1 b1) (R1 b2) = glq b1 b2
  glq _ _ = False
  gbq (L1 a1) (L1 a2) = glq a1 a2
  gbq (R1 b1) (R1 b2) = glq b1 b2
  gbq _ _ = False

instance (GOrd a, GOrd b) => GOrd (a :*: b) where
  glq (a1 :*: b1) (a2 :*: b2) = glq a1 a2 && glq b1 b2
  gbq (a1 :*: b1) (a2 :*: b2) = gbq a1 a2 && gbq b1 b2

instance GOrd a => GOrd (M1 _x _y a) where
  glq (M1 a1) (M1 a2) = glq a1 a2
  gbq (M1 a1) (M1 a2) = gbq a1 a2

genericLq :: (Generic a, GOrd (Rep a)) => a -> a -> Bool
genericLq a b = glq (from a) (from b)


-- only class instance could receive different type parameter agian?  guess right!
-- but here lack one x parameter to be compiant with Generic Type U1,V1 ....
-- with x , you just can represent K1, :+:, and :*:
-- class FindEx a where
--   exNihilp :: Maybe a

-- Just cp from monadplus in Github
class GExNihilo (a :: k -> Type) where
  gExNihilo :: forall (x :: k). Maybe (a x)

instance GExNihilo U1 where
  gExNihilo = Just U1

instance GExNihilo V1 where
  gExNihilo = Nothing

instance GExNihilo (K1 _1 a) where
  gExNihilo = Nothing

instance GExNihilo (a :+: b) where
  gExNihilo = Nothing

instance GExNihilo (a :*: b) where
  gExNihilo = Nothing

instance (GExNihilo a) => GExNihilo (M1 _x _y a) where
  gExNihilo = M1 <$> gExNihilo

exNihilo :: forall a. (Generic a, GExNihilo (Rep a)) => Maybe a
exNihilo = to <$> gExNihilo

class MyEq a where
  eq :: a -> a -> Bool
  default eq :: (Generic a, GEq (Rep a)) -- {-# LANGUAGE DefaultSignatures #-}
    => a
    -> a
    -> Bool
  eq a b = geq (from a) (from b)

data Foo1 a b c
  = G0
  | G1 a
  | G2 b c
  deriving (Generic,MyEq)
 
