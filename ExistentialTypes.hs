{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE ConstraintKinds #-}

-- {-# LANGUAGE ExistentialQuantification #-}
-- | 

module Haskell.ThinkingWithTypes.ExistentialTypes where


import Data.Typeable
import Data.Maybe
import Data.Foldable

import Data.Kind (Constraint, Type)
-- data Any = forall a. Any a

data Any where
  Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

func :: forall a . forall r. (a -> r) -> a -> r
func ar a = ar a

contPlus a b c d = a + b + c + d

sum = contPlus 1 `func` 2 `func` 3 `func` 4

data HasShow where
  HasShow :: Show t =>  t -> HasShow

elimHasShow :: (forall a. Show a => a -> r)  -> HasShow  -> r
elimHasShow f (HasShow a) = f a

-- geta ::  HasShow ->  (forall a. Show a => a)
-- geta (HasShow a) = a

instance Show HasShow where
  show hs = elimHasShow show hs

-- when t converto Dynamic, its complete info is not lost, just hidden beneath Dynamic
data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r)
  -> Dynamic
  -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

-- every new conception, should compose some cases by your own grep to verify if your understanding to them is right  
a = Dynamic (12 :: Float)

-- Nothing
b = fromDynamic a :: Maybe Double

-- Just 12.0
c = fromDynamic a :: Maybe Float

liftD2 :: forall a b r.
  ( Typeable a
  , Typeable b
  , Typeable r
  )
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic

liftD2 d1 d2 f =
  fmap Dynamic . f -- (Dynamic .) . f also works
  <$> fromDynamic @a d1 -- TypeApplications,specify first parameter 'a' of type signature of fromDynamic must be type a here
  <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $ asum
  [ liftD2 @String @String a b (++)
  , liftD2 @Int @Int a b (+)
  , liftD2 @String @Int a b $ \strA intB ->
      strA ++ show intB
  , liftD2 @Int @String a b $ \intA strB ->
      show intA ++ strB
  ]

pyi = fromDynamic @Int (pyPlus ( Dynamic (1 :: Int)) ( Dynamic (2 ::Int)))


data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c  -- here c is Dynamic or Show, Monoid, Monad ...

elimHas
  :: (forall a. c a => a -> r)
  -> Has c
  -> r

elimHas f (Has a) = f a

type ZhenShu = Int

type HasShow2 = Has Show
type Dynamic2 = Has Typeable

