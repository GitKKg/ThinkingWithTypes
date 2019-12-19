-- | 



{-# LANGUAGE FlexibleInstances #-}
-- | 
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Haskell.ThinkingWithTypes.TypefamilyConstrains where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  --(:#) :: t -> HList ts -> HList (t ': ts)
  Link :: t -> HList ts -> HList (t ': ts)
infixr 5 `Link` -- :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ `Link` ts) = 1 + hLength ts

-- type family AllEq (ts :: [Type]) :: Constraint where
--   AllEq '[] = ()
--   AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a `Link` as) == (b `Link` bs) = a == b && as == bs


instance (All Ord ts,All Eq ts) => Ord (HList ts) where
  HNil <= HNil = True
  (a `Link` as) <= (b `Link` bs) = a<=b || as <= bs

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (a `Link` as) = show a ++ " Link " ++ show as
-- a here is of type Constrain Show, so a is could be referred with show a, or say, a here gets a type, refer to tailL in BuidldingTypesFromSchema.hs

ahlist = 12 `Link` "ok" `Link` [1,2] `Link` HNil

--headss ::  HList aas -> (forall a. a =>a )

--headss (a `Link` as ) = a
