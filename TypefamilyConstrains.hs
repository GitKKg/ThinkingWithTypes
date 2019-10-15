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
  (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

-- type family AllEq (ts :: [Type]) :: Constraint where
--   AllEq '[] = ()
--   AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs


instance (All Ord ts,All Eq ts) => Ord (HList ts) where
  HNil <= HNil = True
  (a :# as) <= (b :# bs) = a<=b || as <= bs

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (a :# as) = show a ++ " :# " ++ show as

ahlist = 12 :# "ok" :# [1,2] :# HNil

