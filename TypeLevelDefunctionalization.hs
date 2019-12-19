{-# LANGUAGE TypeOperators #-}
-- | 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskell.ThinkingWithTypes.TypeLevelDefunctionalization where

import Data.Kind (Constraint, Type)

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance Eval (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe _ ( 'Just a )) = a
type instance Eval (FromMaybe  a  'Nothing ) = a

data ListToMaybe :: [a] -> Exp a
type instance Eval (ListToMaybe '[] ) = Nothing
-- {-# LANGUAGE TypeOperators #-} a : la
type instance Eval (ListToMaybe (a ': la)) = Maybe a -- actually : is right too
-- :kind! Eval (ListToMaybe '[Int,Bool] )
-- Eval (ListToMaybe '[Int,Bool] ) :: *
-- = Maybe Int

data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[] ) = '[]
type instance Eval (MapList f (a ': as )) =
  Eval (f a)  ': Eval (MapList f as)  -- actually : is ok too

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr fab '[] b) = b
type instance Eval (Foldr fab (a ': la) b ) = Eval (Foldr fab (Eval (fab a b)) la )
