-- | 
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskell.ThinkingWithTypes.OpenProducts where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Fcf

data Any (f :: k -> Type) where
  Any :: f t -> Any f

-- two lists here,one is type list ts, another one is Vector to store data at term level
data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty
-- f here is not V.Vector

data Key (key :: Symbol) = Key

key = Key @"myData"
-- key :: Key "myData"

-- insert :: Key key
--   -> f t
--   -> OpenProduct f ts
--   -> OpenProduct f ('(key, t) ': ts)

replace0 _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

opV = nil
opVn = replace0 key [] opV

result = replace0 (Key @"key") (Just "hello") opV
-- f here is Maybe , ts here is '[ '("key", [Char])]

result2 = replace0 (Key @"another") (Just True) result
-- f here is Maybe , ts here is '[ '("another", Bool), '("key", [Char])]

-- data (=<<) (c :: a -> Exp b) (d :: Exp a) (e :: b)
--   	-- Defined in ‘Fcf’
-- infixr 1 =<<
-- type instance Eval (k =<< e) = Eval (k (Eval e))
-- data (<=<) (d :: b -> Exp c) (e :: a -> Exp b) (f :: a) (g :: c)
-- type instance Eval ((<=<) f g x) = Eval (f (Eval (g x)))
-- data Filter (b :: a -> Exp Bool) (c :: [a]) (d :: [a])
-- type instance Eval (Filter _p '[]) = '[] 	-- Defined in ‘Fcf’
-- type instance Eval (Filter p (a2 : as))
--   = If (Eval (p a2)) (a2 : Eval (Filter p as)) (Eval (Filter p as))
--   	-- Defined in ‘Fcf’
-- data Fst (c :: (a, b)) (d :: a)
--   	-- Defined in ‘Fcf’
-- type instance Eval (Fst '(a2, _b)) = a2 	-- Defined in ‘Fcf’
-- type family If (b :: Bool) (x :: k) (y :: k) :: k
--   	-- Defined in ‘Fcf’
-- type instance If 'False _x y = y 	-- Defined in ‘Fcf’
-- type instance If 'True x _y = x 	-- Defined in ‘Fcf’
type UniqueKey (key :: k) (ts :: [(k, t)])
  = Null =<< Filter (TyEq key <=< Fst) ts
-- :kind   , below * get no sense?
-- (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> c -> *
-- TyEq :: a -> b -> Bool -> *
-- Filter :: (a -> Exp Bool) -> [a] -> [a] -> *
-- Fst :: (a, b) -> a -> *
-- Null :: [a] -> Bool -> *
-- (=<<) :: (a -> Exp b) -> Exp a -> b -> *
-- equivalent of null . filter (== key) . fst

insert
  :: Eval (UniqueKey key ts) ~ 'True
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)

insert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v
