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
module Haskell.ThinkingWithTypes.HetergeneousList where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs


instance Ord (HList '[]) where
  HNil <= HNil = True

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts )) where
  (a :# as) <= (b :# bs) = a <= b || as <= bs

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts )) where
  show (t  :# ts) = show t ++ show ts


type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)
