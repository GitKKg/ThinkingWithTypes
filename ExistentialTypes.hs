{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE GADTs #-}

{-# LANGUAGE ConstraintKinds #-}

-- :set -XTypeApplications

-- so called ExistentialTypes means general type, not some concrete,rigid type

module Haskell.ThinkingWithTypes.ExistentialTypes where


import Data.Typeable
import Data.Maybe
import Data.Foldable

import Data.Kind (Constraint, Type)

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- data Any = forall a. Any a

data Any where -- {-# LANGUAGE GADTs #-}
  Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r -- {-# LANGUAGE RankNTypes #-}
elimAny f (Any a) = f a

func :: forall a . forall r. (a -> r) -> a -> r
func ar a = ar a

contPlus a b c d = a + b + c + d

sum = contPlus 1 `func` 2 `func` 3 `func` 4

-- HasShow as a value constructor accept t ,which bears Show this constraint ,as a parameter,then return a variable with type HasShow
-- when t conver to HasShow, its complete info is not lost, just hidden beneath HasShow
data HasShow where
  HasShow :: Show t =>  t -> HasShow

showA = HasShow 12

elimHasShow :: (forall a. Show a => a -> r)  -> HasShow  -> r
elimHasShow f (HasShow a) = f a

printA = elimHasShow print showA
-- geta ::  HasShow ->  (forall a. Show a => a)
-- geta (HasShow a) = a

instance Show HasShow where
  show hs = elimHasShow show hs

directPrintA = print showA
directShowA = show showA

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

-- similar to liftA2 , f a b 
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
  <$> fromDynamic @a d1 -- TypeApplications,specify first parameter 'a' of type signature of fromDynamic must be type a here,right, it is first parameter of signature of the fuction but not of fuctions itself
  <*> fromDynamic @b d2 -- {-# LANGUAGE TypeApplications #-} {-# LANGUAGE ScopedTypeVariables #-}

-- :set -XTypeApplications
--  pu a b = liftD2 @String @String a b (++)
--  a = Dynamic "hello"
--  b = Dynamic "boy"
-- fromDynamic (fromJust $ pu a b) :: Maybe String
-- Just "helloboy"

-- asum :: (Foldable t, Alternative f) => t (f a) -> f a

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
-- Just 3

-- data Test = Test Int Int deriving (Show,Eq) ,
-- here Test is a data type ,below Has c is a 'type' type, keyword 'data' can create data type ,also can create 'type' type -> kind
data Has (c :: Type -> Constraint) where -- this Has is type constructor   -- {-# LANGUAGE KindSignatures #-} about Type, {-# LANGUAGE ConstraintKinds #-} about Constaint
  Has :: c t => t -> Has c -- this Has is value constructor

-- Ref below HasT for more clear explaining
-- Hac c is a type created by keyword data, Has as a value constructor is a function accepts a variable t, t bears constraint c t (c t => t) ,and return a type with type of 'Has c'
-- c is a function accept a varibel of type Type, and return a Type Constaint ,or say a Kind Constraint
-- here c may be Monoid, Show, Eq, ... but not Monad for :k Monad is  Monad :: (* -> *) -> Constraint but not  * -> Constraint
-- as type constructor,Hac c is a kind but not a type, as value constructor, the value which create is of type but not kind

-- example:
-- :k Has
-- Has :: (* -> Constraint) -> *
-- :k Has Eq
-- Has Eq :: *
-- :k Eq
-- Eq :: * -> Constraint
-- :k Monad
-- Monad :: (* -> *) -> Constraint
-- :k Constraint
-- Constraint :: *

-- note how we use '=>' ever before : instance Monoid a =>  Applicative (Two a) where

-- Has make us could ref some types class which has same Constraint, like Show,Monoid,Monad


hasInt = Has 1 :: Has Show -- First Has is value constructor,Sencond Has is type constructor

hasString = Has "ok" :: Has Show

elimHas
  :: (forall a. c a => a -> r)
  -> Has c
  -> r

elimHas f (Has a) = f a

-- rename type constructor and value constructor as different names for make conception clear
data HasT (c :: Type -> Constraint) where -- HasT is type constructor
  HasV :: c t => t -> HasT c  -- HasV is valur constructor

type HasShow3 = HasT Show
-- type HasShow4 = HasV Show  -- wrong,HasV is value constructor,delaring type variable must use type constructor
hasString1 = HasV 1 :: HasT Eq
--hasString2 = HasT 1 :: HasT Eq -- wrong, HasT is type constructor ,declaring general variable must use value constructor

-- all the type variables are kind
type ZhenShu = Int

hasBoolList = Has [True] :: Has Show -- The First Has is value constructor, the second Has is Type constructor

th = elimHas show hasBoolList

type HasShow2 = Has Show
type Dynamic2 = Has Typeable

type MonoidAndEq a = (Monoid a, Eq a)
-- :k MonoidAndEq
-- MonoidAndEq :: * -> Constraint

-- note how we us (Constrain,Constaint):
-- instance  (Monoid a, Monoid b ) =>  Applicative (Three a b) where

-- hasBoolList1 = Has [True] :: Has MonoidAndEq -- wrong ,MonoidAndEq lack of a

-- How to compose multiple constraint into one
class (Monoid a,Eq a) => MonoidEq a
instance (Monoid a,Eq a) => MonoidEq a -- {-# LANGUAGE FlexibleInstances #-} and {-# LANGUAGE UndecidableInstances #-}

foo = Has [True] :: Has MonoidEq


